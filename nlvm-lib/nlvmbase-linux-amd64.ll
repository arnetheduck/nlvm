; Workarounds for:
; * bugs in nlvm
; * nim standard library including macros from header files
; * stuff in nimbase.h
; For this to go away, one would have to patch upstream to
; avoid depending on c headers in the stdlib, and fix some bugs
; in nlvm

; stdio.h
@_IOFBF = linkonce_odr constant i32 0
@_IOLBF = linkonce_odr constant i32 1
@_IONBF = linkonce_odr constant i32 2

; time.h
@CLOCK_REALTIME = linkonce_odr constant i32 0
@CLOCKS_PER_SEC = linkonce_odr constant i64 1000000

; unistd.h
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

define zeroext i1 @IN6_IS_ADDR_UNSPECIFIED(i8* nocapture readonly) local_unnamed_addr {
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
define zeroext i1 @IN6_IS_ADDR_LOOPBACK(i8* nocapture readonly) local_unnamed_addr {
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
define zeroext i1 @IN6_IS_ADDR_LINKLOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -25165824)
  %7 = icmp eq i32 %5, %6
  ret i1 %7
}

; Function Attrs: nounwind optsize readonly uwtable
define zeroext i1 @IN6_IS_ADDR_SITELOCAL(i8* nocapture readonly) local_unnamed_addr {
  %2 = bitcast i8* %0 to i32*
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 @llvm.bswap.i32(i32 -4194304)
  %5 = and i32 %4, %3
  %6 = tail call i32 @llvm.bswap.i32(i32 -20971520)
  %7 = icmp eq i32 %5, %6
  ret i1 %7
}

; Function Attrs: nounwind optsize readonly uwtable
define zeroext i1 @IN6_IS_ADDR_V4MAPPED(i8* nocapture readonly) local_unnamed_addr {
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
define zeroext i1 @IN6_IS_ADDR_V4COMPAT(i8* nocapture readonly) local_unnamed_addr {
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
define zeroext i1 @IN6_ARE_ADDR_EQUAL(i8* nocapture readonly, i8* nocapture readonly) local_unnamed_addr {
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

