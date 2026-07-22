## ==-- clang-c/BuildSystem.h - Utilities for use by build systems -*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides various utilities for use by build systems.           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  \defgroup BUILD_SYSTEM Build system utilities
##  @{
##
##
##  Return the timestamp for use with Clang's
##  \c -fbuild-session-timestamp= option.
##

proc getBuildSessionTimestamp*(): culonglong {.
  importc: "clang_getBuildSessionTimestamp", dynlib: CLangLib
.}

##
##  Object encapsulating information about overlaying virtual
##  file/directories over the real file system.
##

type CXVirtualFileOverlay* = ptr cXVirtualFileOverlayImpl

##
##  Create a \c CXVirtualFileOverlay object.
##  Must be disposed with \c clang_VirtualFileOverlay_dispose().
##
##  \param options is reserved, always pass 0.
##

proc virtualFileOverlayCreate*(
  options: cuint
): CXVirtualFileOverlay {.importc: "clang_VirtualFileOverlay_create", dynlib: CLangLib.}

##
##  Map an absolute virtual file path to an absolute real one.
##  The virtual path must be canonicalized (not contain "."/"..").
##  \returns 0 for success, non-zero to indicate an error.
##

proc virtualFileOverlayAddFileMapping*(
  a1: CXVirtualFileOverlay, virtualPath: cstring, realPath: cstring
): CXErrorCode {.importc: "clang_VirtualFileOverlay_addFileMapping", dynlib: CLangLib.}

##
##  Set the case sensitivity for the \c CXVirtualFileOverlay object.
##  The \c CXVirtualFileOverlay object is case-sensitive by default, this
##  option can be used to override the default.
##  \returns 0 for success, non-zero to indicate an error.
##

proc virtualFileOverlaySetCaseSensitivity*(
  a1: CXVirtualFileOverlay, caseSensitive: cint
): CXErrorCode {.
  importc: "clang_VirtualFileOverlay_setCaseSensitivity", dynlib: CLangLib
.}

##
##  Write out the \c CXVirtualFileOverlay object to a char buffer.
##
##  \param options is reserved, always pass 0.
##  \param out_buffer_ptr pointer to receive the buffer pointer, which should be
##  disposed using \c clang_free().
##  \param out_buffer_size pointer to receive the buffer size.
##  \returns 0 for success, non-zero to indicate an error.
##

proc virtualFileOverlayWriteToBuffer*(
  a1: CXVirtualFileOverlay,
  options: cuint,
  outBufferPtr: cstringArray,
  outBufferSize: ptr cuint,
): CXErrorCode {.importc: "clang_VirtualFileOverlay_writeToBuffer", dynlib: CLangLib.}

##
##  free memory allocated by libclang, such as the buffer returned by
##  \c CXVirtualFileOverlay() or \c clang_ModuleMapDescriptor_writeToBuffer().
##
##  \param buffer memory pointer to free.
##

proc free*(buffer: pointer) {.importc: "clang_free", dynlib: CLangLib.}
##
##  Dispose a \c CXVirtualFileOverlay object.
##

proc virtualFileOverlayDispose*(
  a1: CXVirtualFileOverlay
) {.importc: "clang_VirtualFileOverlay_dispose", dynlib: CLangLib.}

##
##  Object encapsulating information about a module.modulemap file.
##

type CXModuleMapDescriptor* = ptr cXModuleMapDescriptorImpl

##
##  Create a \c CXModuleMapDescriptor object.
##  Must be disposed with \c clang_ModuleMapDescriptor_dispose().
##
##  \param options is reserved, always pass 0.
##

proc moduleMapDescriptorCreate*(
  options: cuint
): CXModuleMapDescriptor {.
  importc: "clang_ModuleMapDescriptor_create", dynlib: CLangLib
.}

##
##  Sets the framework module name that the module.modulemap describes.
##  \returns 0 for success, non-zero to indicate an error.
##

proc moduleMapDescriptorSetFrameworkModuleName*(
  a1: CXModuleMapDescriptor, name: cstring
): CXErrorCode {.
  importc: "clang_ModuleMapDescriptor_setFrameworkModuleName", dynlib: CLangLib
.}

##
##  Sets the umbrella header name that the module.modulemap describes.
##  \returns 0 for success, non-zero to indicate an error.
##

proc moduleMapDescriptorSetUmbrellaHeader*(
  a1: CXModuleMapDescriptor, name: cstring
): CXErrorCode {.
  importc: "clang_ModuleMapDescriptor_setUmbrellaHeader", dynlib: CLangLib
.}

##
##  Write out the \c CXModuleMapDescriptor object to a char buffer.
##
##  \param options is reserved, always pass 0.
##  \param out_buffer_ptr pointer to receive the buffer pointer, which should be
##  disposed using \c clang_free().
##  \param out_buffer_size pointer to receive the buffer size.
##  \returns 0 for success, non-zero to indicate an error.
##

proc moduleMapDescriptorWriteToBuffer*(
  a1: CXModuleMapDescriptor,
  options: cuint,
  outBufferPtr: cstringArray,
  outBufferSize: ptr cuint,
): CXErrorCode {.importc: "clang_ModuleMapDescriptor_writeToBuffer", dynlib: CLangLib.}

##
##  Dispose a \c CXModuleMapDescriptor object.
##

proc moduleMapDescriptorDispose*(
  a1: CXModuleMapDescriptor
) {.importc: "clang_ModuleMapDescriptor_dispose", dynlib: CLangLib.}

##
##  @}
##
