# Gala
Library of parralel programming for Delphi/FPC

The implementation of Gala library is based on low-level abilities of parallel programming provided by Win (Windows operational systems) - threads, signals, mutexes, semaphores, critical sections, messages. The library allows to perform task decomposition using async parallel processes and shared resources working within one Win application. Delphi provides TThread class but its functionality is mostly limited to coworking with VCL/LCL that are not thread-safe. TThread does not contain any methods of coworking with other parallel processes. This class is useful when in the program there are few statically created processes (threads) working with main VCL(LCL)-thread and not coworking. If a task can be divided into quite a big number of dynamically created, freed and coworking processes, the use of TThread becomes very complicated and unreliable. The use of diverse low-level Win sync-features proper is extremely hard, pregnant with faults and unmanageable (though more efficient).
Gala library allows to code using the terms of async parallel processes and their coworking - almost as conveniently as in Ocaml and Ada - with async rendezvous for synchronization. The main classes implemented in Gala are processes, containers, channels, signals and messages. Special attention is paid to such security issues as correct termination of suspended threads and error handling in already terminated threads.

Gala is Freeware.

Gala was tested on Delphi 4, 5, 6, 7/FPC 3.0.0 on Windows 95, 98, NT, 2000, XP,7,8,10.


For examlpes compilation just run Lazarus ide, open project
  Examples\GalaExamples.lpr and build it.

Do not run the application on the debugger, because multithread application is poorly working in debug mode.

Original author is

Sergey Gurin. Russian Federation, Tomsk
gurin@mail.tomsknet.ru
http://www.tomsknet.ru/gala.html

Ported on LAzarus + FPC by Cynic.

