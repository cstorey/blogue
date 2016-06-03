---
date: '2014-09-30'
orig_url: http://www.lshift.net/blog/2014/09/30/defn-reloaded-main
title: (defn reloaded/-main [] …)
description: The code is dead, long live the code.
---

I’ve been using [Stuart Sierra’s reloaded
pattern](http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded)
on a few Clojure projects recently, and I hit upon the problem of how
best to use Stuart Sierra’s reloaded with a typical long running
application. So, I thought I’d share one possible solution.

Most of the code should be self explanatory, but there are a few things
worth calling out. Starting (logically) from the end, I wanted to be
able to run the process under a process manager like upstart, and from
the terminal, and have the process shut-down when it gets sent `SIGTERM`
or `SIGINT` (ie: from upstart or from Ctrl-C in the terminal). We use a
runtime-shut-down hook, by registering an (un-started) `Thread` to be
invoked with the JVM, which will be invoked when the JVM shuts shutting
down, and use that to clean up our running system.

At the end of our main function, we can usually assume that the
application will be running in background threads. However, quite often,
these are all daemonised, and hence won’t prevent the JVM from shutting
down when `-main` has returned. Hence, we call `run-forever`, which
creates a semaphore with no permits, and then try to acquire one.
Because the semaphore is essentially empty, this causes the thread to
wait.

Setting up the system is reasonably self explanatory–I use the example
of using an EDN configuration file, here, but it’s equally possible to
extract the values directly from the command line, or the environment
(perhaps using the [environ](https://github.com/weavejester/environ)
module).

    (ns horse.main
       (:require
                 [clojure.tools.logging :as log]
                 [clojure.java.io :as io]
                 [clojure.edn :as edn]
                 [com.stuartsierra.component :as component])
       (:gen-class))

     (defn add-shutdown-hook! [^Runnable f]
       (.addShutdownHook (Runtime/getRuntime)
         (Thread. f)))

     (defn logged-shutdown [system]
       (log/info ::shutting-down)
       (swap! system component/stop)
       (log/info ::shutdown-done))

     (defn run-forever []
       (let [forever (java.util.concurrent.Semaphore. 0)]
         (.acquire forever)))

     (defn make-system [{:keys [stuff] :as config}]
       (component/system-map
          ;; :thing (map->Thingy {:stuff stuff})
        ))

     (defn -main [configfile & argv]
       (log/info ::booting-from configfile)
       (let [config (-> (io/reader configfile) slurp edn/read-string)
             sys (atom (make-system config))]
         (add-shutdown-hook! (partial logged-shutdown sys))

         (log/info ::starting)
         (swap! sys component/start)
         (log/info ::running)
         (run-forever)))
