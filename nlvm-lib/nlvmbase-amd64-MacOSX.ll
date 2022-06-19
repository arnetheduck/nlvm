; Workarounds for:
; * bugs in nlvm
; * nim standard library including macros from header files
; * stuff in nimbase.h
; For this to go away, one would have to patch upstream to
; avoid depending on c headers in the stdlib, and fix some bugs
; in nlvm

target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-macos"

; ioctls.h
@FIONCLEX = linkonce_odr constant i32 21584
@FIOCLEX = linkonce_odr constant i32 21585

; pthreads.h
@PTHREAD_MUTEX_RECURSIVE = linkonce_odr constant i32 1

; signal.h
@SIG_ERR = global void (i32)* inttoptr (i32 -1 to void (i32)*), align 8
@SIG_DFL = global void (i32)* inttoptr (i32 0 to void (i32)*), align 8
@SIG_IGN = global void (i32)* inttoptr (i32 1 to void (i32)*), align 8
@SEGV_MAPERR = linkonce_odr constant i32 1
@SIGEV_NONE = linkonce_odr constant i32 1
@SIGEV_SIGNAL = linkonce_odr constant i32 2
@SIGEV_THREAD = linkonce_odr constant i32 3
@SIGABRT = linkonce_odr constant i32 6
@SIGALRM = linkonce_odr constant i32 14
@SIGBUS = linkonce_odr constant i32 10
@SIGCHLD = linkonce_odr constant i32 20
@SIGCONT = linkonce_odr constant i32 19
@SIGFPE = linkonce_odr constant i32 8
@SIGHUP = linkonce_odr constant i32 1
@SIGILL = linkonce_odr constant i32 4
@SIGINT = linkonce_odr constant i32 2
@SIGKILL = linkonce_odr constant i32 9
@SIGPIPE = linkonce_odr constant i32 13
@SIGQUIT = linkonce_odr constant i32 3
@SIGSEGV = linkonce_odr constant i32 11
@SIGSTOP = linkonce_odr constant i32 17
@SIGTERM = linkonce_odr constant i32 15
@SIGTSTP = linkonce_odr constant i32 18
@SIGTTIN = linkonce_odr constant i32 21
@SIGTTOU = linkonce_odr constant i32 22
@SIGUSR1 = linkonce_odr constant i32 30
@SIGUSR2 = linkonce_odr constant i32 31
@SIGPOLL = linkonce_odr constant i32 23
@SIGPROF = linkonce_odr constant i32 27
@SIGSYS = linkonce_odr constant i32 12
@SIGTRAP = linkonce_odr constant i32 5
@SIGURG = linkonce_odr constant i32 16
@SIGVTALRM = linkonce_odr constant i32 26
@SIGXCPU = linkonce_odr constant i32 24
@SIGXFSZ = linkonce_odr constant i32 25
@SA_NOCLDSTOP = linkonce_odr constant i32 1
@SIG_BLOCK = linkonce_odr constant i32 1
@SIG_UNBLOCK = linkonce_odr constant i32 2
@SIG_SETMASK = linkonce_odr constant i32 0
@SS_ONSTACK = linkonce_odr constant i32 1
@SS_DISABLE = linkonce_odr constant i32 2
@MINSIGSTKSZ = linkonce_odr constant i32 2048
@SIGSTKSZ = linkonce_odr constant i32 8192

; errno.h
@EACCES = linkonce_odr constant i32 13
@EADDRINUSE = linkonce_odr constant i32 48
@EADDRNOTAVAIL = linkonce_odr constant i32 49
@EAFNOSUPPORT = linkonce_odr constant i32 47
@EAGAIN = linkonce_odr constant i32 35
@EALREADY = linkonce_odr constant i32 37
@EBADF = linkonce_odr constant i32 9
@EBADMSG = linkonce_odr constant i32 94
@EBUSY = linkonce_odr constant i32 16
@ECANCELED = linkonce_odr constant i32 89
@ECHILD = linkonce_odr constant i32 10
@ECONNABORTED = linkonce_odr constant i32 53
@ECONNREFUSED = linkonce_odr constant i32 61
@ECONNRESET = linkonce_odr constant i32 54
@EDEADLK = linkonce_odr constant i32 11
@EDESTADDRREQ = linkonce_odr constant i32 39
@EDOM = linkonce_odr constant i32 33
@EDQUOT = linkonce_odr constant i32 69
@EEXIST = linkonce_odr constant i32 17
@EFAULT = linkonce_odr constant i32 14
@EFBIG = linkonce_odr constant i32 27
@EHOSTUNREACH = linkonce_odr constant i32 65
@EIDRM = linkonce_odr constant i32 90
@EILSEQ = linkonce_odr constant i32 92
@EINPROGRESS = linkonce_odr constant i32 36
@EINTR = linkonce_odr constant i32 4
@EINVAL = linkonce_odr constant i32 22
@EIO = linkonce_odr constant i32 5
@EISCONN = linkonce_odr constant i32 56
@EISDIR = linkonce_odr constant i32 21
@ELOOP = linkonce_odr constant i32 62
@EMFILE = linkonce_odr constant i32 24
@EMLINK = linkonce_odr constant i32 31
@EMSGSIZE = linkonce_odr constant i32 40
@EMULTIHOP = linkonce_odr constant i32 95
@ENAMETOOLONG = linkonce_odr constant i32 63
@ENETDOWN = linkonce_odr constant i32 50
@ENETRESET = linkonce_odr constant i32 52
@ENETUNREACH = linkonce_odr constant i32 51
@ENFILE = linkonce_odr constant i32 23
@ENOBUFS = linkonce_odr constant i32 55
@ENODATA = linkonce_odr constant i32 96
@ENODEV = linkonce_odr constant i32 19
@ENOENT = linkonce_odr constant i32 2
@ENOEXEC = linkonce_odr constant i32 8
@ENOLCK = linkonce_odr constant i32 77
@ENOLINK = linkonce_odr constant i32 97
@ENOMEM = linkonce_odr constant i32 12
@ENOMSG = linkonce_odr constant i32 91
@ENOPROTOOPT = linkonce_odr constant i32 42
@ENOSPC = linkonce_odr constant i32 28
@ENOSR = linkonce_odr constant i32 98
@ENOSTR = linkonce_odr constant i32 99
@ENOSYS = linkonce_odr constant i32 78
@ENOTCONN = linkonce_odr constant i32 57
@ENOTDIR = linkonce_odr constant i32 20
@ENOTEMPTY = linkonce_odr constant i32 66
@ENOTSOCK = linkonce_odr constant i32 38
@ENOTSUP = linkonce_odr constant i32 45
@ENOTTY = linkonce_odr constant i32 25
@ENXIO = linkonce_odr constant i32 6
@EOPNOTSUPP = linkonce_odr constant i32 102
@EOVERFLOW = linkonce_odr constant i32 84
@EPERM = linkonce_odr constant i32 1
@EPIPE = linkonce_odr constant i32 32
@EPROTO = linkonce_odr constant i32 100
@EPROTONOSUPPORT = linkonce_odr constant i32 43
@EPROTOTYPE = linkonce_odr constant i32 41
@ERANGE = linkonce_odr constant i32 34
@EROFS = linkonce_odr constant i32 30
@ESPIPE = linkonce_odr constant i32 29
@ESRCH = linkonce_odr constant i32 3
@ESTALE = linkonce_odr constant i32 70
@ETIME = linkonce_odr constant i32 101
@ETIMEDOUT = linkonce_odr constant i32 60
@ETXTBSY = linkonce_odr constant i32 26
@EWOULDBLOCK = linkonce_odr constant i32 35
@EXDEV = linkonce_odr constant i32 18

; spawn.h
@POSIX_SPAWN_RESETIDS = linkonce_odr constant i32 1
@POSIX_SPAWN_SETPGROUP = linkonce_odr constant i32 2
@POSIX_SPAWN_SETSCHEDPARAM = linkonce_odr constant i32 16
@POSIX_SPAWN_SETSCHEDULER = linkonce_odr constant i32 32
@POSIX_SPAWN_SETSIGDEF = linkonce_odr constant i32 4
@POSIX_SPAWN_SETSIGMASK = linkonce_odr constant i32 8
@POSIX_SPAWN_USEVFORK = linkonce_odr constant i32 64

; sys/stat.h
@S_IFBLK = linkonce_odr constant i32 24576
@S_IFCHR = linkonce_odr constant i32 8192
@S_IFDIR = linkonce_odr constant i32 16384
@S_IFIFO = linkonce_odr constant i32 4096
@S_IFLNK = linkonce_odr constant i32 40960
@S_IFMT = linkonce_odr constant i32 61440
@S_IFREG = linkonce_odr constant i32 32768
@S_IFSOCK = linkonce_odr constant i32 49152
@S_IRGRP = linkonce_odr constant i32 32
@S_IROTH = linkonce_odr constant i32 4
@S_IRUSR = linkonce_odr constant i32 256
@S_IRWXG = linkonce_odr constant i32 56
@S_IRWXO = linkonce_odr constant i32 7
@S_IRWXU = linkonce_odr constant i32 448
@S_ISGID = linkonce_odr constant i32 1024
@S_ISUID = linkonce_odr constant i32 2048
@S_ISVTX = linkonce_odr constant i32 512
@S_IWGRP = linkonce_odr constant i32 16
@S_IWOTH = linkonce_odr constant i32 2
@S_IWUSR = linkonce_odr constant i32 128
@S_IXGRP = linkonce_odr constant i32 8
@S_IXOTH = linkonce_odr constant i32 1
@S_IXUSR = linkonce_odr constant i32 64 

; stdio.h
@_IOFBF = linkonce_odr constant i32 0
@_IOLBF = linkonce_odr constant i32 1
@_IONBF = linkonce_odr constant i32 2
@L_ctermid = linkonce_odr constant i32 9

; syscall.h
@SYS_getrandom = linkonce_odr constant i32 318

; sys/ioctl.h
@TIOCGWINSZ = linkonce_odr constant i32 21523

; time.h
@CLOCK_REALTIME = linkonce_odr constant i32 0
@CLOCKS_PER_SEC = linkonce_odr constant i64 1000000

; termios.h
@VINTR = linkonce_odr constant i32 0
@VQUIT = linkonce_odr constant i32 1
@VERASE = linkonce_odr constant i32 2
@VKILL = linkonce_odr constant i32 3
@VEOF = linkonce_odr constant i32 4
@VTIME = linkonce_odr constant i32 5
@VMIN = linkonce_odr constant i32 6
@VSTART = linkonce_odr constant i32 8
@VSTOP = linkonce_odr constant i32 9
@VSUSP = linkonce_odr constant i32 10
@VEOL = linkonce_odr constant i32 11
@IGNBRK = linkonce_odr constant i32 1
@BRKINT = linkonce_odr constant i32 2
@IGNPAR = linkonce_odr constant i32 4
@PARMRK = linkonce_odr constant i32 8
@INPCK = linkonce_odr constant i32 16
@ISTRIP = linkonce_odr constant i32 32
@INLCR = linkonce_odr constant i32 64
@IGNCR = linkonce_odr constant i32 128
@ICRNL = linkonce_odr constant i32 256
@IUCLC = linkonce_odr constant i32 512
@IXON = linkonce_odr constant i32 1024
@IXANY = linkonce_odr constant i32 2048
@IXOFF = linkonce_odr constant i32 4096
@OPOST = linkonce_odr constant i32 1
@ONLCR = linkonce_odr constant i32 4
@OCRNL = linkonce_odr constant i32 8
@ONOCR = linkonce_odr constant i32 16
@ONLRET = linkonce_odr constant i32 32
@OFILL = linkonce_odr constant i32 64
@OFDEL = linkonce_odr constant i32 128
@NLDLY = linkonce_odr constant i32 256
@NL0 = linkonce_odr constant i32 0
@NL1 = linkonce_odr constant i32 256
@CRDLY = linkonce_odr constant i32 1536
@CR0 = linkonce_odr constant i32 0
@CR1 = linkonce_odr constant i32 512
@CR2 = linkonce_odr constant i32 1024
@CR3 = linkonce_odr constant i32 1536
@TABDLY = linkonce_odr constant i32 6144
@TAB0 = linkonce_odr constant i32 0
@TAB1 = linkonce_odr constant i32 2048
@TAB2 = linkonce_odr constant i32 4096
@TAB3 = linkonce_odr constant i32 6144
@BSDLY = linkonce_odr constant i32 8192
@BS0 = linkonce_odr constant i32 0
@BS1 = linkonce_odr constant i32 8192
@FFDLY = linkonce_odr constant i32 32768
@FF0 = linkonce_odr constant i32 0
@FF1 = linkonce_odr constant i32 32768
@VTDLY = linkonce_odr constant i32 16384
@VT0 = linkonce_odr constant i32 0
@VT1 = linkonce_odr constant i32 16384
@B0 = linkonce_odr constant i32 0
@B50 = linkonce_odr constant i32 1
@B75 = linkonce_odr constant i32 2
@B110 = linkonce_odr constant i32 3
@B134 = linkonce_odr constant i32 4
@B150 = linkonce_odr constant i32 5
@B200 = linkonce_odr constant i32 6
@B300 = linkonce_odr constant i32 7
@B600 = linkonce_odr constant i32 8
@B1200 = linkonce_odr constant i32 9
@B1800 = linkonce_odr constant i32 10
@B2400 = linkonce_odr constant i32 11
@B4800 = linkonce_odr constant i32 12
@B9600 = linkonce_odr constant i32 13
@B19200 = linkonce_odr constant i32 14
@B38400 = linkonce_odr constant i32 15
@EXTA = linkonce_odr constant i32 14
@EXTB = linkonce_odr constant i32 15
@CSIZE = linkonce_odr constant i32 48
@CS5 = linkonce_odr constant i32 0
@CS6 = linkonce_odr constant i32 16
@CS7 = linkonce_odr constant i32 32
@CS8 = linkonce_odr constant i32 48
@CSTOPB = linkonce_odr constant i32 64
@CREAD = linkonce_odr constant i32 128
@PARENB = linkonce_odr constant i32 256
@PARODD = linkonce_odr constant i32 512
@HUPCL = linkonce_odr constant i32 1024
@CLOCAL = linkonce_odr constant i32 2048
@ISIG = linkonce_odr constant i32 1
@ICANON = linkonce_odr constant i32 2
@ECHO = linkonce_odr constant i32 8
@ECHOE = linkonce_odr constant i32 16
@ECHOK = linkonce_odr constant i32 32
@ECHONL = linkonce_odr constant i32 64
@NOFLSH = linkonce_odr constant i32 128
@TOSTOP = linkonce_odr constant i32 256
@IEXTEN = linkonce_odr constant i32 32768
@TCOOFF = linkonce_odr constant i32 0
@TCOON = linkonce_odr constant i32 1
@TCIOFF = linkonce_odr constant i32 2
@TCION = linkonce_odr constant i32 3
@TCIFLUSH = linkonce_odr constant i32 0
@TCOFLUSH = linkonce_odr constant i32 1
@TCIOFLUSH = linkonce_odr constant i32 2
@TCSANOW = linkonce_odr constant i32 0
@TCSADRAIN = linkonce_odr constant i32 1
@TCSAFLUSH = linkonce_odr constant i32 2

; fcntl.h
@F_GETFD = linkonce_odr constant i32 1
@F_SETFD = linkonce_odr constant i32 2
@FD_CLOEXEC = linkonce_odr constant i32 1

; sys/wait.h
@WNOHANG = linkonce_odr constant i32 1
@WUNTRACED = linkonce_odr constant i32 2
@WEXITED = linkonce_odr constant i32 4
@WSTOPPED = linkonce_odr constant i32 2
@WCONTINUED = linkonce_odr constant i32 8
@WNOWAIT = linkonce_odr constant i32 16777216

; copyfile.h
@COPYFILE_ACL	= linkonce_odr constant i32 1
@COPYFILE_STAT = linkonce_odr constant i32 2
@COPYFILE_XATTR	= linkonce_odr constant i32 4
@COPYFILE_DATA = linkonce_odr constant i32 8

; sys/wait.h
; proc WEXITSTATUS*(s: cint): cint =  (s and 0xff00) shr 8
; proc WTERMSIG*(s:cint): cint = s and 0x7f
; proc WSTOPSIG*(s:cint): cint = WEXITSTATUS(s)
; proc WIFEXITED*(s:cint) : bool = WTERMSIG(s) == 0
; proc WIFSIGNALED*(s:cint) : bool = (cast[int8]((s and 0x7f) + 1) shr 1) > 0
; proc WIFSTOPPED*(s:cint) : bool = (s and 0xff) == 0x7f
; proc WIFCONTINUED*(s:cint) : bool = s == WCONTINUED

define linkonce_odr i32 @WEXITSTATUS(i32 %m) {
  %1 = and i32 %m, 65280
  %2 = ashr i32 %1, 8
  ret i32 %2
}

define linkonce_odr i32 @WTERMSIG(i32 %m) {
  %1 = and i32 %m, 127
  ret i32 %1
}

; unistd.h
define linkonce_odr i8 @S_ISDIR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 16384
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISCHR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 8192
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISBLK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 24576
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISREG(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 32768
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISFIFO(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 4096
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISLNK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 40960
  %3 = zext i1 %2 to i8
  ret i8 %3
}
define linkonce_odr i8 @S_ISSOCK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 49152
  %3 = zext i1 %2 to i8
  ret i8 %3
}
; sys/select.h
%fd_set = type { [16 x i64] }

define linkonce_odr void @FD_ZERO(%fd_set* %0) {
  %2 = alloca %fd_set*, align 8
  %3 = alloca i32, align 4
  %4 = alloca %fd_set*, align 8
  store %fd_set* %0, %fd_set** %2, align 8
  br label %5

5:                                                ; preds = %1
  %6 = load %fd_set*, %fd_set** %2, align 8
  store %fd_set* %6, %fd_set** %4, align 8
  store i32 0, i32* %3, align 4
  br label %7

7:                                                ; preds = %17, %5
  %8 = load i32, i32* %3, align 4
  %9 = zext i32 %8 to i64
  %10 = icmp ult i64 %9, 16
  br i1 %10, label %11, label %20

11:                                               ; preds = %7
  %12 = load %fd_set*, %fd_set** %4, align 8
  %13 = getelementptr inbounds %fd_set, %fd_set* %12, i32 0, i32 0
  %14 = load i32, i32* %3, align 4
  %15 = zext i32 %14 to i64
  %16 = getelementptr inbounds [16 x i64], [16 x i64]* %13, i64 0, i64 %15
  store i64 0, i64* %16, align 8
  br label %17

17:                                               ; preds = %11
  %18 = load i32, i32* %3, align 4
  %19 = add i32 %18, 1
  store i32 %19, i32* %3, align 4
  br label %7

20:                                               ; preds = %7
  br label %21

21:                                               ; preds = %20
  ret void
}

define linkonce_odr void @FD_SET(i32 %0, %fd_set* %1) {
  %3 = alloca i32, align 4
  %4 = alloca %fd_set*, align 8
  store i32 %0, i32* %3, align 4
  store %fd_set* %1, %fd_set** %4, align 8
  %5 = load i32, i32* %3, align 4
  %6 = srem i32 %5, 64
  %7 = zext i32 %6 to i64
  %8 = shl i64 1, %7
  %9 = load %fd_set*, %fd_set** %4, align 8
  %10 = getelementptr inbounds %fd_set, %fd_set* %9, i32 0, i32 0
  %11 = load i32, i32* %3, align 4
  %12 = sdiv i32 %11, 64
  %13 = sext i32 %12 to i64
  %14 = getelementptr inbounds [16 x i64], [16 x i64]* %10, i64 0, i64 %13
  %15 = load i64, i64* %14, align 8
  %16 = or i64 %15, %8
  store i64 %16, i64* %14, align 8
  ret void
}

define linkonce_odr i32 @FD_ISSET(i32 %0, %fd_set* %1) {
  %3 = alloca i32, align 4
  %4 = alloca %fd_set*, align 8
  store i32 %0, i32* %3, align 4
  store %fd_set* %1, %fd_set** %4, align 8
  %5 = load %fd_set*, %fd_set** %4, align 8
  %6 = getelementptr inbounds %fd_set, %fd_set* %5, i32 0, i32 0
  %7 = load i32, i32* %3, align 4
  %8 = sdiv i32 %7, 64
  %9 = sext i32 %8 to i64
  %10 = getelementptr inbounds [16 x i64], [16 x i64]* %6, i64 0, i64 %9
  %11 = load i64, i64* %10, align 8
  %12 = load i32, i32* %3, align 4
  %13 = srem i32 %12, 64
  %14 = zext i32 %13 to i64
  %15 = shl i64 1, %14
  %16 = and i64 %11, %15
  %17 = icmp ne i64 %16, 0
  %18 = zext i1 %17 to i32
  ret i32 %18
}

define linkonce_odr i8 @WIFEXITED(i32 %m) {
  %1 = and i32 %m, 127
  %2 = icmp eq i32 %1, 0
  %3 = zext i1 %2 to i8
  ret i8 %3
}

define linkonce_odr i8 @WIFSIGNALED(i32 %m) {
  %1 = shl i32 %m, 24
  %2 = and i32 %1, 2130706432
  %3 = add nuw i32 %2, 16777216
  %4 = ashr i32 %3, 25
  %5 = icmp sgt i32 %4, 0
  %6 = zext i1 %5 to i8
  ret i8 %6
}
; nimbase.h

declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i32, i1)

define linkonce_odr void @zeroMem(i8* %p, i64 %s) {
  call void @llvm.memset.p0i8.i64(i8* %p, i8 0, i64 %s, i32 0, i1 false)
  ret void
}

declare i32 @memcmp(i8*, i8*, i64)

define linkonce_odr i8 @equalMem(i8* %a, i8* %b, i64 %l) {
  %1 = call i32 @memcmp(i8* %a, i8* %b, i64 %l)
  %2 = icmp eq i32 %1, 0
  %3 = zext i1 %2 to i8
  ret i8 %3
}

define linkonce_odr i1 @likely(i1 %a) {
  ret i1 %a
}

define linkonce_odr i1 @unlikely(i1 %a) {
  ret i1 %a
}

declare i32 @llvm.bswap.i32(i32)

define linkonce_odr zeroext i8 @IN6_IS_ADDR_UNSPECIFIED(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %20

; <label>:5:                                      ; preds = %1
  %6 = getelementptr inbounds i8, i8* %0, i64 4
  %7 = bitcast i8* %6 to i32*
  %8 = load i32, i32* %7, align 4
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %20

; <label>:10:                                     ; preds = %5
  %11 = getelementptr inbounds i8, i8* %0, i64 8
  %12 = bitcast i8* %11 to i32*
  %13 = load i32, i32* %12, align 4
  %14 = icmp eq i32 %13, 0
  br i1 %14, label %15, label %20

; <label>:15:                                     ; preds = %10
  %16 = getelementptr inbounds i8, i8* %0, i64 12
  %17 = bitcast i8* %16 to i32*
  %18 = load i32, i32* %17, align 4
  %19 = icmp eq i32 %18, 0
  br label %20

; <label>:20:                                     ; preds = %15, %10, %5, %1
  %21 = phi i1 [ false, %10 ], [ false, %5 ], [ false, %1 ], [ %19, %15 ]
  %22 = zext i1 %21 to i8
  ret i8 %22
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_IS_ADDR_LOOPBACK(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %21

; <label>:5:                                      ; preds = %1
  %6 = getelementptr inbounds i8, i8* %0, i64 4
  %7 = bitcast i8* %6 to i32*
  %8 = load i32, i32* %7, align 4
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %21

; <label>:10:                                     ; preds = %5
  %11 = getelementptr inbounds i8, i8* %0, i64 8
  %12 = bitcast i8* %11 to i32*
  %13 = load i32, i32* %12, align 4
  %14 = icmp eq i32 %13, 0
  br i1 %14, label %15, label %21

; <label>:15:                                     ; preds = %10
  %16 = getelementptr inbounds i8, i8* %0, i64 12
  %17 = bitcast i8* %16 to i32*
  %18 = load i32, i32* %17, align 4
  %19 = tail call i32 @llvm.bswap.i32(i32 1)
  %20 = icmp eq i32 %18, %19
  br label %21

; <label>:21:                                     ; preds = %15, %10, %5, %1
  %22 = phi i1 [ false, %10 ], [ false, %5 ], [ false, %1 ], [ %20, %15 ]
  %23 = zext i1 %22 to i8
  ret i8 %23
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_IS_ADDR_LINKLOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -25165824)
  %7 = icmp eq i32 %5, %6
  %8 = zext i1 %7 to i8
  ret i8 %8
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_IS_ADDR_SITELOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -20971520)
  %7 = icmp eq i32 %5, %6
  %8 = zext i1 %7 to i8
  ret i8 %8
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_IS_ADDR_V4MAPPED(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %16

; <label>:5:                                      ; preds = %1
  %6 = getelementptr inbounds i8, i8* %0, i64 4
  %7 = bitcast i8* %6 to i32*
  %8 = load i32, i32* %7, align 4
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %16

; <label>:10:                                     ; preds = %5
  %11 = getelementptr inbounds i8, i8* %0, i64 8
  %12 = bitcast i8* %11 to i32*
  %13 = load i32, i32* %12, align 4
  %14 = tail call i32 @llvm.bswap.i32(i32 65535)
  %15 = icmp eq i32 %13, %14
  br label %16

; <label>:16:                                     ; preds = %10, %5, %1
  %17 = phi i1 [ false, %5 ], [ false, %1 ], [ %15, %10 ]
  %18 = zext i1 %17 to i8
  ret i8 %18
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_IS_ADDR_V4COMPAT(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %21

; <label>:5:                                      ; preds = %1
  %6 = getelementptr inbounds i8, i8* %0, i64 4
  %7 = bitcast i8* %6 to i32*
  %8 = load i32, i32* %7, align 4
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %21

; <label>:10:                                     ; preds = %5
  %11 = getelementptr inbounds i8, i8* %0, i64 8
  %12 = bitcast i8* %11 to i32*
  %13 = load i32, i32* %12, align 4
  %14 = icmp eq i32 %13, 0
  br i1 %14, label %15, label %21

; <label>:15:                                     ; preds = %10
  %16 = getelementptr inbounds i8, i8* %0, i64 12
  %17 = bitcast i8* %16 to i32*
  %18 = load i32, i32* %17, align 4
  %19 = tail call i32 @llvm.bswap.i32(i32 %18)
  %20 = icmp ugt i32 %19, 1
  br label %21

; <label>:21:                                     ; preds = %15, %10, %5, %1
  %22 = phi i1 [ false, %10 ], [ false, %5 ], [ false, %1 ], [ %20, %15 ]
  %23 = zext i1 %22 to i8
  ret i8 %23
}

; Function Attrs: norecurse nounwind optsize readonly uwtable
define linkonce_odr zeroext i8 @IN6_ARE_ADDR_EQUAL(i8* nocapture readonly, i8* nocapture readonly) local_unnamed_addr {
  %3 = bitcast i8* %0 to i32*
  %4 = load i32, i32* %3, align 4
  %5 = bitcast i8* %1 to i32*
  %6 = load i32, i32* %5, align 4
  %7 = icmp eq i32 %4, %6
  br i1 %7, label %8, label %32

; <label>:8:                                      ; preds = %2
  %9 = getelementptr inbounds i8, i8* %0, i64 4
  %10 = bitcast i8* %9 to i32*
  %11 = load i32, i32* %10, align 4
  %12 = getelementptr inbounds i8, i8* %1, i64 4
  %13 = bitcast i8* %12 to i32*
  %14 = load i32, i32* %13, align 4
  %15 = icmp eq i32 %11, %14
  br i1 %15, label %16, label %32

; <label>:16:                                     ; preds = %8
  %17 = getelementptr inbounds i8, i8* %0, i64 8
  %18 = bitcast i8* %17 to i32*
  %19 = load i32, i32* %18, align 4
  %20 = getelementptr inbounds i8, i8* %1, i64 8
  %21 = bitcast i8* %20 to i32*
  %22 = load i32, i32* %21, align 4
  %23 = icmp eq i32 %19, %22
  br i1 %23, label %24, label %32

; <label>:24:                                     ; preds = %16
  %25 = getelementptr inbounds i8, i8* %0, i64 12
  %26 = bitcast i8* %25 to i32*
  %27 = load i32, i32* %26, align 4
  %28 = getelementptr inbounds i8, i8* %1, i64 12
  %29 = bitcast i8* %28 to i32*
  %30 = load i32, i32* %29, align 4
  %31 = icmp eq i32 %27, %30
  br label %32

; <label>:32:                                     ; preds = %24, %16, %8, %2
  %33 = phi i1 [ false, %16 ], [ false, %8 ], [ false, %2 ], [ %31, %24 ]
  %34 = zext i1 %33 to i8
  ret i8 %34
}
