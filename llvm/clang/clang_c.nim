## clang-c wrappers include file
## Generated from clang/clang-c headers using c2nim

import std/time_t
type TimeT = Time

type
  # Opaque handles to various things
  cXVirtualFileOverlayImpl {.pure, final.} = object
  cXModuleMapDescriptorImpl {.pure, final.} = object
  cXTargetInfoImpl {.pure, final.} = object
  cXTranslationUnitImpl {.pure, final.} = object
  cXCursorSetImpl {.pure, final.} = object
  cXAPISetImpl {.pure, final.} = object
  CXIndex = pointer
  CXIndexOptions = pointer

## Order matters - CXString must be included first as other headers depend on it
include CXString
include CXFile
include CXSourceLocation
include CXDiagnostic
include CXErrorCode
include CXCompilationDatabase

type CXIndexOptionsCXErrorCode = CXErrorCode

include BuildSystem
include Index ## Must be before Documentation (CXCursor)
include Documentation
include FatalErrorHandler
include Rewrite
