---
date: '2017-09-30'
orig_url: https://tech.labs.oliverwyman.com/blog/2017/10/04/a-quick-tour-of-llvms-sanitizer-coverage/
title: "A quick tour of LLVM's Sanitizer coverage implementation"
description: "Inside the grey box"
---

After reading about [hypothesis](https://github.com/HypothesisWorks/hypothesis-python)'s new coverage features, I've recently become interested in how guided fuzzing (as implemented by [American Fuzzy Lop](http://lcamtuf.coredump.cx/afl/) or LLVM's [libFuzzer](http://llvm.org/docs/LibFuzzer.html) works internally with Rust and LLVM. The first step is to understand how coverage works.  <!--more-->

Clang's [Sanitizer Coverage](http://clang.llvm.org/docs/SanitizerCoverage.html) documentation explains the functionality very well, so I'll not repeat too much of that.

First of all, I started off by looking at the Rust Fuzz project's set of [targets](https://github.com/rust-fuzz/targets). The [run-fuzzer.sh](https://github.com/rust-fuzz/targets/blob/a12ab636b54ce3e5cf19cbae38dc2913ad52dd43/run-fuzzer.sh) driver script tells cargo to pass several extra flags to the compiler. The flag `-C passes=sancov` instructs the compiler to also run the `sancov` compiler pass, which annotates the generated code to add calls into the coverage runtime, and `-C llvm-args=-sanitizer-coverage-level=3` instructs LLVM to record [edge coverage](http://clang.llvm.org/docs/SanitizerCoverage.html#edge-coverage) so that we can tell what paths of code executed (e.g.: differentiating between branches of an if/else expression). The additional `-Z sanitizer=address` also tells the compiler to link in the sanitizer support runtime, which includes the routines to record and save coverage.

We'll start with a trivial program in `main.rs`:

```rust
#[inline(never)]
fn show(a: &str) {
    println!("{}", a);
}

fn main() {
    use std::env::args;
    for a in args() {
        show(&a)
    }
}
```

If we compile this with `RUSTFLAGS=' -C passes=sancov -C llvm-args=-sanitizer-coverage-level=3 -Z sanitizer=address' cargo run` and then look at the resulting disassembled code, using `objdump -CS target/debug/covtest` ^[this assumes the GNU BinUtils suite; commonly used on Linux. Other systems will likely have similar tools.], then we see an additional set of lines like:

```
10465:       48 8d 05 24 86 34 00    lea    0x348624(%rip),%rax
1046c:       48 05 d4 03 00 00       add    $0x3d4,%rax
10472:       48 89 c7                mov    %rax,%rdi
10475:       e8 56 68 0e 00          callq  f6cd0 <__sanitizer_cov>
```

Granted, I'm not great at reading assembly, but this looks to lookup the current program counter^[i.e.: the instruction that was running at the time], massages it a little to create a guard address, and passes that as the first argument to the [`__sanitizer_cov` function](https://github.com/llvm-project/llvm-project-20170507/blob/c0c70fd0d42d0344aa7c45c8edd2a823745275b0/compiler-rt/lib/sanitizer_common/sanitizer_coverage_libcdep.cc#L935-L938).

This looks up the caller's current program counter, then passes that into [`CoverageData::Add`](https://github.com/llvm-project/llvm-project-20170507/blob/c0c70fd0d42d0344aa7c45c8edd2a823745275b0/compiler-rt/lib/sanitizer_common/sanitizer_coverage_libcdep.cc#L407-L422), which checks uses the guard to check if that point has already been recorded. If not, it'll record the program counter for later storage.

This all gets setup by the [global constructors](http://llvm.org/docs/FAQ.html#what-is-this-llvm-global-ctors-and-global-i-a-stuff-that-happens-when-i-include-iostream), the same mechanism uses to call constructors for static objects in C++. This synthesises a function named `sancov.module_ctor` that then calls [`__sanitizer_cov_module_init`](https://github.com/llvm-project/llvm-project-20170507/blob/c0c70fd0d42d0344aa7c45c8edd2a823745275b0/compiler-rt/lib/sanitizer_common/sanitizer_coverage_libcdep.cc#L962-L973); which allocates space and sets up the coverage data structures. The sanitizer runtime will also ensure that if needed, [`__sanitizer_cov_dump`](https://github.com/llvm-project/llvm-project-20170507/blob/c0c70fd0d42d0344aa7c45c8edd2a823745275b0/compiler-rt/lib/sanitizer_common/sanitizer_coverage_libcdep.cc#L955-L960) is called when the process exits; so that the coverage information will get saved to disk, and later analysed.

So code coverage is one of those things that can seem somewhat magical; mostly because modern compilers can seem awfully complex (and in fairness, they do an awful lot); but the nuts and bolts of it aren't that complicated in themselves.

LLVM does have the very cool feature that it’s possible to provide your own implementation of the coverage interface, allowing you to do customized, very detailed tracing of your program, if you want to do fancier things like analyzing the exact control flow of your program. But that’s an exercise for another day.
