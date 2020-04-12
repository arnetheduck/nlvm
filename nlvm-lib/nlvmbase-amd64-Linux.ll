; Workarounds for:
; * bugs in nlvm
; * nim standard library including macros from header files
; * stuff in nimbase.h
; For this to go away, one would have to patch upstream to
; avoid depending on c headers in the stdlib, and fix some bugs
; in nlvm

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; stdio.h
@_IOFBF = linkonce_odr constant i32 0
@_IOLBF = linkonce_odr constant i32 1
@_IONBF = linkonce_odr constant i32 2
@L_ctermid = linkonce_odr constant i32 9

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
@EINTR = linkonce_odr constant i32 4


; unistd.h
define linkonce_odr i1 @S_ISDIR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 16384
  ret i1 %2
}
define linkonce_odr i1 @S_ISCHR(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 8192
  ret i1 %2
}
define linkonce_odr i1 @S_ISBLK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 24576
  ret i1 %2
}
define linkonce_odr i1 @S_ISREG(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 32768
  ret i1 %2
}
define linkonce_odr i1 @S_ISFIFO(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 4096
  ret i1 %2
}
define linkonce_odr i1 @S_ISLNK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 40960
  ret i1 %2
}
define linkonce_odr i1 @S_ISSOCK(i32 %m) {
  %1 = and i32 %m, 61440
  %2 = icmp eq i32 %1, 49152
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

define linkonce_odr i1 @WIFEXITED(i32 %m) {
  %1 = and i32 %m, 127
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}

define linkonce_odr i1 @WIFSIGNALED(i32 %m) {
  %1 = shl i32 %m, 24
  %2 = and i32 %1, 2130706432
  %3 = add nuw i32 %2, 16777216
  %4 = ashr i32 %3, 25
  %5 = icmp sgt i32 %4, 0
  ret i1 %5
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

define linkonce_odr i1 @likely(i1 %a) {
  ret i1 %a
}

define linkonce_odr i1 @unlikely(i1 %a) {
  ret i1 %a
}

declare i32 @llvm.bswap.i32(i32)

define linkonce_odr zeroext i1 @IN6_IS_ADDR_UNSPECIFIED(i8* nocapture readonly) local_unnamed_addr {
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
  ret i1 %21
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_IS_ADDR_LOOPBACK(i8* nocapture readonly) local_unnamed_addr {
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
  ret i1 %22
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_IS_ADDR_LINKLOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -25165824)
  %7 = icmp eq i32 %5, %6
  ret i1 %7
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_IS_ADDR_SITELOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -20971520)
  %7 = icmp eq i32 %5, %6
  ret i1 %7
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_IS_ADDR_V4MAPPED(i8* nocapture readonly) local_unnamed_addr {
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
  ret i1 %17
}

; Function Attrs: nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_IS_ADDR_V4COMPAT(i8* nocapture readonly) local_unnamed_addr {
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
  ret i1 %22
}

; Function Attrs: norecurse nounwind optsize readonly uwtable
define linkonce_odr zeroext i1 @IN6_ARE_ADDR_EQUAL(i8* nocapture readonly, i8* nocapture readonly) local_unnamed_addr {
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
  ret i1 %33
}

