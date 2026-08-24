= What is Nix?
As it's core, nix is a reproducable build system. Just like how the core abstraction the UNIX system is a file and the core 
abstraction of functional languages is a function, nix also has a core abstraction: the derivation. I think to really
understand a system - you _have to understand the core abstraction_!

== The Derivation Datastructure
Imagine a build script, let's say, for a simple hello world C program:
```c
#include <stdio.h>
 
int main()
{
 
    printf("Hello World");
    return 0;
}
```

Compiling this is easy right? Just do:
```

```

But this process will not be _reproducable across machines_! What if you are running Linux but your friend is running MacOs?
Or if on one machine, you had a different version of `gcc` and there's a _compiler bug_ which caused your program to execute differently than your
other machine?

Nix makes this process _reproducable_ by forcing you to make an _exact version of gcc_  as an input to the build script! The reproducable 
formula for building software in nix is called the _derivation_.

= Nix the package manager: What does a package manager do?
== The Filesystem Hierachy Standard
== The nix package manager
