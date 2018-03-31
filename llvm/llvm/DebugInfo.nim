## ===------------ DebugInfo.h - LLVM C API Debug Info API -----------------===//
##
##                      The LLVM Compiler Infrastructure
##
##  This file is distributed under the University of Illinois Open Source
##  License. See LICENSE.TXT for details.
##
## ===----------------------------------------------------------------------===//
## /
## / This file declares the C API endpoints for generating DWARF Debug Info
## /
## / Note: This interface is experimental. It is *NOT* stable, and may be
## /       changed without warning.
## /
## ===----------------------------------------------------------------------===//

## *
##  Debug info flags.
##

type
  DIFlags* {.size: sizeof(cint).} = enum
    DIFlagZero = 0, DIFlagPrivate = 1, DIFlagProtected = 2, DIFlagPublic = 3,
    DIFlagFwdDecl = 1 shl 2, DIFlagAppleBlock = 1 shl 3, DIFlagBlockByrefStruct = 1 shl 4,
    DIFlagVirtual = 1 shl 5, DIFlagArtificial = 1 shl 6, DIFlagExplicit = 1 shl 7,
    DIFlagPrototyped = 1 shl 8, DIFlagObjcClassComplete = 1 shl 9,
    DIFlagObjectPointer = 1 shl 10, DIFlagVector = 1 shl 11,
    DIFlagStaticMember = 1 shl 12, DIFlagLValueReference = 1 shl 13,
    DIFlagRValueReference = 1 shl 14, DIFlagReserved = 1 shl 15,
    DIFlagSingleInheritance = 1 shl 16, DIFlagMultipleInheritance = 2 shl 16,
    DIFlagVirtualInheritance = 3 shl 16, DIFlagIntroducedVirtual = 1 shl 18,
    DIFlagBitField = 1 shl 19, DIFlagNoReturn = 1 shl 20, DIFlagMainSubprogram = 1 shl 21,
#    DIFlagIndirectVirtualBase = (1 shl 2) or (1 shl 5),
#    DIFlagAccessibility = dIFlagPrivate or dIFlagProtected or dIFlagPublic, DIFlagPtrToMemberRep = dIFlagSingleInheritance or
#        dIFlagMultipleInheritance or dIFlagVirtualInheritance


## *
##  Source languages known by DWARF.
##

type
  DWARFSourceLanguage* {.size: sizeof(cint).} = enum
    DWARFSourceLanguageC89, DWARFSourceLanguageC, DWARFSourceLanguageAda83,
    DWARFSourceLanguageC_plusPlus, DWARFSourceLanguageCobol74,
    DWARFSourceLanguageCobol85, DWARFSourceLanguageFortran77,
    DWARFSourceLanguageFortran90, DWARFSourceLanguagePascal83, DWARFSourceLanguageModula2, ##  New in DWARF v3:
    DWARFSourceLanguageJava, DWARFSourceLanguageC99, DWARFSourceLanguageAda95,
    DWARFSourceLanguageFortran95, DWARFSourceLanguagePLI, DWARFSourceLanguageObjC,
    DWARFSourceLanguageObjC_plusPlus, DWARFSourceLanguageUPC, DWARFSourceLanguageD, ##  New in DWARF v4:
    DWARFSourceLanguagePython, ##  New in DWARF v5:
    DWARFSourceLanguageOpenCL, DWARFSourceLanguageGo, DWARFSourceLanguageModula3,
    DWARFSourceLanguageHaskell, DWARFSourceLanguageC_plusPlus03,
    DWARFSourceLanguageC_plusPlus11, DWARFSourceLanguageOCaml,
    DWARFSourceLanguageRust, DWARFSourceLanguageC11, DWARFSourceLanguageSwift,
    DWARFSourceLanguageJulia, DWARFSourceLanguageDylan,
    DWARFSourceLanguageC_plusPlus14, DWARFSourceLanguageFortran03,
    DWARFSourceLanguageFortran08, DWARFSourceLanguageRenderScript, DWARFSourceLanguageBLISS, ##  Vendor extensions:
    DWARFSourceLanguageMipsAssembler, DWARFSourceLanguageGOOGLE_RenderScript,
    DWARFSourceLanguageBORLAND_Delphi


## *
##  The amount of debug information to emit.
##

type
  DWARFEmissionKind* {.size: sizeof(cint).} = enum
    DWARFEmissionNone = 0, DWARFEmissionFull, DWARFEmissionLineTablesOnly


## *
##  The current debug metadata version number.
##

proc debugMetadataVersion*(): cuint {.importc: "LLVMDebugMetadataVersion",
                                   dynlib: LLVMLib.}
## *
##  The version of debug metadata that's present in the provided \c Module.
##

proc getModuleDebugMetadataVersion*(module: ModuleRef): cuint {.
    importc: "LLVMGetModuleDebugMetadataVersion", dynlib: LLVMLib.}
## *
##  Strip debug info in the module if it exists.
##  To do this, we remove all calls to the debugger intrinsics and any named
##  metadata for debugging. We also remove debug locations for instructions.
##  Return true if module is modified.
##

proc stripModuleDebugInfo*(module: ModuleRef): Bool {.
    importc: "LLVMStripModuleDebugInfo", dynlib: LLVMLib.}
## *
##  Construct a builder for a module, and do not allow for unresolved nodes
##  attached to the module.
##

proc createDIBuilderDisallowUnresolved*(m: ModuleRef): DIBuilderRef {.
    importc: "LLVMCreateDIBuilderDisallowUnresolved", dynlib: LLVMLib.}
## *
##  Construct a builder for a module and collect unresolved nodes attached
##  to the module in order to resolve cycles during a call to
##  \c LLVMDIBuilderFinalize.
##

proc createDIBuilder*(m: ModuleRef): DIBuilderRef {.importc: "LLVMCreateDIBuilder",
    dynlib: LLVMLib.}
## *
##  Deallocates the \c DIBuilder and everything it owns.
##  @note You must call \c LLVMDIBuilderFinalize before this
##

proc disposeDIBuilder*(builder: DIBuilderRef) {.importc: "LLVMDisposeDIBuilder",
    dynlib: LLVMLib.}
## *
##  Construct any deferred debug info descriptors.
##

proc dIBuilderFinalize*(builder: DIBuilderRef) {.importc: "LLVMDIBuilderFinalize",
    dynlib: LLVMLib.}
## *
##  A CompileUnit provides an anchor for all debugging
##  information generated during this instance of compilation.
##  \param Lang          Source programming language, eg.
##                       \c LLVMDWARFSourceLanguageC99
##  \param FileRef       File info.
##  \param Producer      Identify the producer of debugging information
##                       and code.  Usually this is a compiler
##                       version string.
##  \param ProducerLen   The length of the C string passed to \c Producer.
##  \param isOptimized   A boolean flag which indicates whether optimization
##                       is enabled or not.
##  \param Flags         This string lists command line options. This
##                       string is directly embedded in debug info
##                       output which may be used by a tool
##                       analyzing generated debugging information.
##  \param FlagsLen      The length of the C string passed to \c Flags.
##  \param RuntimeVer    This indicates runtime version for languages like
##                       Objective-C.
##  \param SplitName     The name of the file that we'll split debug info
##                       out into.
##  \param SplitNameLen  The length of the C string passed to \c SplitName.
##  \param Kind          The kind of debug information to generate.
##  \param DWOId         The DWOId if this is a split skeleton compile unit.
##  \param SplitDebugInlining    Whether to emit inline debug info.
##  \param DebugInfoForProfiling Whether to emit extra debug info for
##                               profile collection.
##

proc dIBuilderCreateCompileUnit*(builder: DIBuilderRef; lang: DWARFSourceLanguage;
                                fileRef: MetadataRef; producer: cstring;
                                producerLen: csize; isOptimized: Bool;
                                flags: cstring; flagsLen: csize; runtimeVer: cuint;
                                splitName: cstring; splitNameLen: csize;
                                kind: DWARFEmissionKind; dWOId: cuint;
                                splitDebugInlining: Bool;
                                debugInfoForProfiling: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateCompileUnit", dynlib: LLVMLib.}
## *
##  Create a file descriptor to hold debugging information for a file.
##  \param Builder      The \c DIBuilder.
##  \param Filename     File name.
##  \param FilenameLen  The length of the C string passed to \c Filename.
##  \param Directory    Directory.
##  \param DirectoryLen The length of the C string passed to \c Directory.
##

proc dIBuilderCreateFile*(builder: DIBuilderRef; filename: cstring;
                         filenameLen: csize; directory: cstring; directoryLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateFile", dynlib: LLVMLib.}
## *
##  Creates a new DebugLocation that describes a source location.
##  \param Line The line in the source file.
##  \param Column The column in the source file.
##  \param Scope The scope in which the location resides.
##  \param InlinedAt The scope where this location was inlined, if at all.
##                   (optional).
##  \note If the item to which this location is attached cannot be
##        attributed to a source line, pass 0 for the line and column.
##

proc dIBuilderCreateDebugLocation*(ctx: ContextRef; line: cuint; column: cuint;
                                  scope: MetadataRef; inlinedAt: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateDebugLocation", dynlib: LLVMLib.}