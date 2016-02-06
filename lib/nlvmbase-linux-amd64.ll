; Workarounds for:
; * bugs in nlvm
; * nim standard library including macros from header files
; * stuff in nimbase.h
; For this to go away, one would have to patch upstream to
; avoid depending on c headers in the stdlib, and fix some bugs
; in nlvm

; dlfcn.h
@RTLD_NOW = global i32 2

; errno.h
@EEXIST = global i32 17
@ENOENT = global i32 2

; limits.h
@PATH_MAX = global i32 4096

; signal.h
@SIGABRT = global i32 6
@SIGCONT = global i32 18
@SIGFPE = global i32 8
@SIGILL = global i32 4
@SIGINT = global i32 2
@SIGKILL = global i32 9
@SIGPIPE = global i32 13
@SIGSEGV = global i32 11
@SIGSTOP = global i32 19
@SIGTERM = global i32 15

; stdio.h
@_IOFBF = global i32 0
@_IOLBF = global i32 1
@_IONBF = global i32 2

; time.h
@CLOCKS_PER_SEC = global i64 1000000

; unistd.h
@_SC_NPROCESSORS_ONLN = global i32 84
@F_SETFD = global i32 2

; sys/mman.h
@MAP_ANONYMOUS = global i32 32

; sys/stat.h
@S_IRWXU = global i32 448
@S_IRUSR = global i32 256
@S_IWUSR = global i32 128
@S_IXUSR = global i32 64

@S_IRWXG = global i32 56
@S_IRGRP = global i32 32
@S_IWGRP = global i32 16
@S_IXGRP = global i32 8

@S_IRWXO = global i32 7
@S_IROTH = global i32 4
@S_IWOTH = global i32 2
@S_IXOTH = global i32 1

define i1 @S_ISDIR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 16384
  ret i1 %2
}
define i1 @S_ISLNK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 40960
  ret i1 %2
}
define i1 @S_ISREG(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 32768
  ret i1 %2
}

; sys/select.h
%fd_set = type opaque

define void @FD_ZERO(%fd_set*) {
  ret void
}
define void @FD_SET(i32, %fd_set*) {
  ret void
}
define i32 @FD_ISSET(i32, %fd_set*) {
  ret i32 0
}

; sys/wait.h
@WNOHANG = global i32 1

define i1 @WIFEXITED(i32 %m) {
  %1 = and i32 %m, 127
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}

; nimbase.h

declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i32, i1)

define void @zeroMem(i8* %p, i64 %s) {
  call void @llvm.memset.p0i8.i64(i8* %p, i8 0, i64 %s, i32 0, i1 false)
  ret void
}

declare i32 @memcmp(i8*, i8*, i64)

define i1 @equalMem(i8* %a, i8* %b, i64 %l) {
  %1 = call i32 @memcmp(i8* %a, i8* %b, i64 %l)
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}
