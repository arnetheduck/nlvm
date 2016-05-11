; Workarounds for:
; * bugs in nlvm
; * nim standard library including macros from header files
; * stuff in nimbase.h
; For this to go away, one would have to patch upstream to
; avoid depending on c headers in the stdlib, and fix some bugs
; in nlvm

; dlfcn.h
@RTLD_NOW = linkonce_odr constant i32 2

; errno.h
@EBADF = linkonce_odr constant i32 9
@EEXIST = linkonce_odr constant i32 17
@EINTR = linkonce_odr constant i32 4
@ENOENT = linkonce_odr constant i32 2

; fcntl.h
@O_RDONLY = linkonce_odr constant i32 0
@O_RDWR = linkonce_odr constant i32 2

@O_CREAT = linkonce_odr constant i32 64
@O_TRUNC = linkonce_odr constant i32 512

@FD_CLOEXEC = linkonce_odr constant i32 1

; limits.h
@PATH_MAX = linkonce_odr constant i32 4096

; signal.h
@SIGABRT = linkonce_odr constant i32 6
@SIGCONT = linkonce_odr constant i32 18
@SIGFPE = linkonce_odr constant i32 8
@SIGILL = linkonce_odr constant i32 4
@SIGINT = linkonce_odr constant i32 2
@SIGKILL = linkonce_odr constant i32 9
@SIGPIPE = linkonce_odr constant i32 13
@SIGSEGV = linkonce_odr constant i32 11
@SIGSTOP = linkonce_odr constant i32 19
@SIGTERM = linkonce_odr constant i32 15

; stdio.h
@_IOFBF = linkonce_odr constant i32 0
@_IOLBF = linkonce_odr constant i32 1
@_IONBF = linkonce_odr constant i32 2

; time.h
@CLOCKS_PER_SEC = linkonce_odr constant i64 1000000

; unistd.h
@_SC_NPROCESSORS_ONLN = linkonce_odr constant i32 84
@F_SETFD = linkonce_odr constant i32 2

; sys/mman.h
@MAP_ANONYMOUS = linkonce_odr constant i32 32
@MAP_PRIVATE = linkonce_odr constant i32 2
@MAP_POPULATE = linkonce_odr constant i32 32768
@MAP_SHARED = linkonce_odr constant i32 1
@MAP_FAILED = linkonce_odr constant i32 -1

@PROT_READ = linkonce_odr constant i32 1
@PROT_WRITE = linkonce_odr constant i32 2

; sys/stat.h
@S_IRWXU = linkonce_odr constant i32 448
@S_IRUSR = linkonce_odr constant i32 256
@S_IWUSR = linkonce_odr constant i32 128
@S_IXUSR = linkonce_odr constant i32 64

@S_IRWXG = linkonce_odr constant i32 56
@S_IRGRP = linkonce_odr constant i32 32
@S_IWGRP = linkonce_odr constant i32 16
@S_IXGRP = linkonce_odr constant i32 8

@S_IRWXO = linkonce_odr constant i32 7
@S_IROTH = linkonce_odr constant i32 4
@S_IWOTH = linkonce_odr constant i32 2
@S_IXOTH = linkonce_odr constant i32 1

define linkonce_odr i1 @S_ISDIR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 16384
  ret i1 %2
}
define linkonce_odr i1 @S_ISLNK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 40960
  ret i1 %2
}
define linkonce_odr i1 @S_ISREG(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 32768
  ret i1 %2
}

; sys/select.h
%fd_set = type opaque

define linkonce_odr void @FD_ZERO(%fd_set*) {
  ret void
}
define linkonce_odr void @FD_SET(i32, %fd_set*) {
  ret void
}
define linkonce_odr i32 @FD_ISSET(i32, %fd_set*) {
  ret i32 0
}

; sys/socket.h
@AF_INET = linkonce_odr constant i32 2
@AF_INET6 = linkonce_odr constant i32 10

; sys/wait.h
@WNOHANG = linkonce_odr constant i32 1

define linkonce_odr i1 @WIFEXITED(i32 %m) {
  %1 = and i32 %m, 127
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}

; nimbase.h

declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i32, i1)

define linkonce_odr void @zeroMem(i8* %p, i64 %s) {
  call void @llvm.memset.p0i8.i64(i8* %p, i8 0, i64 %s, i32 0, i1 false)
  ret void
}

declare i32 @memcmp(i8*, i8*, i64)

define linkonce_odr i1 @equalMem(i8* %a, i8* %b, i64 %l) {
  %1 = call i32 @memcmp(i8* %a, i8* %b, i64 %l)
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}
