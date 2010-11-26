# mutrace



This is a little tool I came across by Lennart Poettering for profiling pthread mutexes.  For each mutex in the program it tells you how many times the mutex was locked, how often there was contention, and the maximum time the mutex was held locked for, amongst other things.



Very useful for diagnosing lock contention in the RTS.


- [ mutrace](http://git.0pointer.de/?p=mutrace.git;a=summary)
