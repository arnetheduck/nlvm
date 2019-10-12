## ===------------ DebugInfo.h - LLVM C API Debug Info API -----------------===//
##
##  Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
##  See https://llvm.org/LICENSE.txt for license information.
##  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
    DIFlagBitField = 1 shl 19, DIFlagNoReturn = 1 shl 20,
    DIFlagTypePassByValue = 1 shl 22, DIFlagTypePassByReference = 1 shl 23,
    DIFlagEnumClass = 1 shl 24,
    DIFlagThunk = 1 shl 25, DIFlagNonTrivial = 1 shl 26, DIFlagBigEndian = 1 shl 27,
    DIFlagLittleEndian = 1 shl 28,
    #DIFlagIndirectVirtualBase = (1 shl 2) or (1 shl 5),
    #DIFlagAccessibility = dIFlagPrivate or dIFlagProtected or dIFlagPublic, DIFlagPtrToMemberRep = dIFlagSingleInheritance or
    #    dIFlagMultipleInheritance or dIFlagVirtualInheritance


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
##  The kind of metadata nodes.
##

const
  MDStringMetadataKind* = 0
  ConstantAsMetadataMetadataKind* = 1
  LocalAsMetadataMetadataKind* = 2
  DistinctMDOperandPlaceholderMetadataKind* = 3
  MDTupleMetadataKind* = 4
  DILocationMetadataKind* = 5
  DIExpressionMetadataKind* = 6
  DIGlobalVariableExpressionMetadataKind* = 7
  GenericDINodeMetadataKind* = 8
  DISubrangeMetadataKind* = 9
  DIEnumeratorMetadataKind* = 10
  DIBasicTypeMetadataKind* = 11
  DIDerivedTypeMetadataKind* = 12
  DICompositeTypeMetadataKind* = 13
  DISubroutineTypeMetadataKind* = 14
  DIFileMetadataKind* = 15
  DICompileUnitMetadataKind* = 16
  DISubprogramMetadataKind* = 17
  DILexicalBlockMetadataKind* = 18
  DILexicalBlockFileMetadataKind* = 19
  DINamespaceMetadataKind* = 20
  DIModuleMetadataKind* = 21
  DITemplateTypeParameterMetadataKind* = 22
  DITemplateValueParameterMetadataKind* = 23
  DIGlobalVariableMetadataKind* = 24
  DILocalVariableMetadataKind* = 25
  DILabelMetadataKind* = 26
  DIObjCPropertyMetadataKind* = 27
  DIImportedEntityMetadataKind* = 28
  DIMacroMetadataKind* = 29
  DIMacroFileMetadataKind* = 30
  DICommonBlockMetadataKind* = 31

type
  MetadataKind* = cuint

## *
##  An LLVM DWARF type encoding.
##

type
  DWARFTypeEncoding* = cuint

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
##  Creates a new descriptor for a module with the specified parent scope.
##  \param Builder         The \c DIBuilder.
##  \param ParentScope     The parent scope containing this module declaration.
##  \param Name            Module name.
##  \param NameLen         The length of the C string passed to \c Name.
##  \param ConfigMacros    A space-separated shell-quoted list of -D macro
##                           definitions as they would appear on a command line.
##  \param ConfigMacrosLen The length of the C string passed to \c ConfigMacros.
##  \param IncludePath     The path to the module map file.
##  \param IncludePathLen  The length of the C string passed to \c IncludePath.
##  \param ISysRoot        The Clang system root (value of -isysroot).
##  \param ISysRootLen     The length of the C string passed to \c ISysRoot.
##

proc dIBuilderCreateModule*(builder: DIBuilderRef; parentScope: MetadataRef;
                           name: cstring; nameLen: csize; configMacros: cstring;
                           configMacrosLen: csize; includePath: cstring;
                           includePathLen: csize; iSysRoot: cstring;
                           iSysRootLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateModule", dynlib: LLVMLib.}
## *
##  Creates a new descriptor for a namespace with the specified parent scope.
##  \param Builder          The \c DIBuilder.
##  \param ParentScope      The parent scope containing this module declaration.
##  \param Name             NameSpace name.
##  \param NameLen          The length of the C string passed to \c Name.
##  \param ExportSymbols    Whether or not the namespace exports symbols, e.g.
##                          this is true of C++ inline namespaces.
##

proc dIBuilderCreateNameSpace*(builder: DIBuilderRef; parentScope: MetadataRef;
                              name: cstring; nameLen: csize; exportSymbols: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateNameSpace", dynlib: LLVMLib.}
## *
##  Create a new descriptor for the specified subprogram.
##  \param Builder         The \c DIBuilder.
##  \param Scope           Function scope.
##  \param Name            Function name.
##  \param NameLen         Length of enumeration name.
##  \param LinkageName     Mangled function name.
##  \param LinkageNameLen  Length of linkage name.
##  \param File            File where this variable is defined.
##  \param LineNo          Line number.
##  \param Ty              Function type.
##  \param IsLocalToUnit   True if this function is not externally visible.
##  \param IsDefinition    True if this is a function definition.
##  \param ScopeLine       Set to the beginning of the scope this starts
##  \param Flags           E.g.: \c LLVMDIFlagLValueReference. These flags are
##                         used to emit dwarf attributes.
##  \param IsOptimized     True if optimization is ON.
##

proc dIBuilderCreateFunction*(builder: DIBuilderRef; scope: MetadataRef;
                             name: cstring; nameLen: csize; linkageName: cstring;
                             linkageNameLen: csize; file: MetadataRef;
                             lineNo: cuint; ty: MetadataRef; isLocalToUnit: Bool;
                             isDefinition: Bool; scopeLine: cuint; flags: DIFlags;
                             isOptimized: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateFunction", dynlib: LLVMLib.}
## *
##  Create a descriptor for a lexical block with the specified parent context.
##  \param Builder      The \c DIBuilder.
##  \param Scope        Parent lexical block.
##  \param File         Source file.
##  \param Line         The line in the source file.
##  \param Column       The column in the source file.
##

proc dIBuilderCreateLexicalBlock*(builder: DIBuilderRef; scope: MetadataRef;
                                 file: MetadataRef; line: cuint; column: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateLexicalBlock", dynlib: LLVMLib.}
## *
##  Create a descriptor for a lexical block with a new file attached.
##  \param Builder        The \c DIBuilder.
##  \param Scope          Lexical block.
##  \param File           Source file.
##  \param Discriminator  DWARF path discriminator value.
##

proc dIBuilderCreateLexicalBlockFile*(builder: DIBuilderRef; scope: MetadataRef;
                                     file: MetadataRef; discriminator: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateLexicalBlockFile", dynlib: LLVMLib.}
## *
##  Create a descriptor for an imported namespace. Suitable for e.g. C++
##  using declarations.
##  \param Builder    The \c DIBuilder.
##  \param Scope      The scope this module is imported into
##  \param File       File where the declaration is located.
##  \param Line       Line number of the declaration.
##

proc dIBuilderCreateImportedModuleFromNamespace*(builder: DIBuilderRef;
    scope: MetadataRef; ns: MetadataRef; file: MetadataRef; line: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromNamespace", dynlib: LLVMLib.}
## *
##  Create a descriptor for an imported module that aliases another
##  imported entity descriptor.
##  \param Builder        The \c DIBuilder.
##  \param Scope          The scope this module is imported into
##  \param ImportedEntity Previous imported entity to alias.
##  \param File           File where the declaration is located.
##  \param Line           Line number of the declaration.
##

proc dIBuilderCreateImportedModuleFromAlias*(builder: DIBuilderRef;
    scope: MetadataRef; importedEntity: MetadataRef; file: MetadataRef; line: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromAlias", dynlib: LLVMLib.}
## *
##  Create a descriptor for an imported module.
##  \param Builder    The \c DIBuilder.
##  \param Scope      The scope this module is imported into
##  \param M          The module being imported here
##  \param File       File where the declaration is located.
##  \param Line       Line number of the declaration.
##

proc dIBuilderCreateImportedModuleFromModule*(builder: DIBuilderRef;
    scope: MetadataRef; m: MetadataRef; file: MetadataRef; line: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromModule", dynlib: LLVMLib.}
## *
##  Create a descriptor for an imported function, type, or variable.  Suitable
##  for e.g. FORTRAN-style USE declarations.
##  \param Builder    The DIBuilder.
##  \param Scope      The scope this module is imported into.
##  \param Decl       The declaration (or definition) of a function, type,
##                      or variable.
##  \param File       File where the declaration is located.
##  \param Line       Line number of the declaration.
##  \param Name       A name that uniquely identifies this imported declaration.
##  \param NameLen    The length of the C string passed to \c Name.
##

proc dIBuilderCreateImportedDeclaration*(builder: DIBuilderRef; scope: MetadataRef;
                                        decl: MetadataRef; file: MetadataRef;
                                        line: cuint; name: cstring; nameLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedDeclaration", dynlib: LLVMLib.}
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
## *
##  Get the line number of this debug location.
##  \param Location     The debug location.
##
##  @see DILocation::getLine()
##

proc dILocationGetLine*(location: MetadataRef): cuint {.
    importc: "LLVMDILocationGetLine", dynlib: LLVMLib.}
## *
##  Get the column number of this debug location.
##  \param Location     The debug location.
##
##  @see DILocation::getColumn()
##

proc dILocationGetColumn*(location: MetadataRef): cuint {.
    importc: "LLVMDILocationGetColumn", dynlib: LLVMLib.}
## *
##  Get the local scope associated with this debug location.
##  \param Location     The debug location.
##
##  @see DILocation::getScope()
##

proc dILocationGetScope*(location: MetadataRef): MetadataRef {.
    importc: "LLVMDILocationGetScope", dynlib: LLVMLib.}
## *
##  Get the "inline at" location associated with this debug location.
##  \param Location     The debug location.
##
##  @see DILocation::getInlinedAt()
##

proc dILocationGetInlinedAt*(location: MetadataRef): MetadataRef {.
    importc: "LLVMDILocationGetInlinedAt", dynlib: LLVMLib.}
## *
##  Get the metadata of the file associated with a given scope.
##  \param Scope     The scope object.
##
##  @see DIScope::getFile()
##

proc dIScopeGetFile*(scope: MetadataRef): MetadataRef {.
    importc: "LLVMDIScopeGetFile", dynlib: LLVMLib.}
## *
##  Get the directory of a given file.
##  \param File     The file object.
##  \param Len      The length of the returned string.
##
##  @see DIFile::getDirectory()
##

proc dIFileGetDirectory*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetDirectory", dynlib: LLVMLib.}
## *
##  Get the name of a given file.
##  \param File     The file object.
##  \param Len      The length of the returned string.
##
##  @see DIFile::getFilename()
##

proc dIFileGetFilename*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetFilename", dynlib: LLVMLib.}
## *
##  Get the source of a given file.
##  \param File     The file object.
##  \param Len      The length of the returned string.
##
##  @see DIFile::getSource()
##

proc dIFileGetSource*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetSource", dynlib: LLVMLib.}
## *
##  Create a type array.
##  \param Builder        The DIBuilder.
##  \param Data           The type elements.
##  \param NumElements    Number of type elements.
##

proc dIBuilderGetOrCreateTypeArray*(builder: DIBuilderRef; data: ptr MetadataRef;
                                   numElements: csize): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateTypeArray", dynlib: LLVMLib.}
## *
##  Create subroutine type.
##  \param Builder        The DIBuilder.
##  \param File            The file in which the subroutine resides.
##  \param ParameterTypes  An array of subroutine parameter types. This
##                         includes return type at 0th index.
##  \param NumParameterTypes The number of parameter types in \c ParameterTypes
##  \param Flags           E.g.: \c LLVMDIFlagLValueReference.
##                         These flags are used to emit dwarf attributes.
##

proc dIBuilderCreateSubroutineType*(builder: DIBuilderRef; file: MetadataRef;
                                   parameterTypes: ptr MetadataRef;
                                   numParameterTypes: cuint; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateSubroutineType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for an enumerator.
##  @param Builder        The DIBuilder.
##  @param Name           Enumerator name.
##  @param NameLen        Length of enumerator name.
##  @param Value          Enumerator value.
##  @param IsUnsigned     True if the value is unsigned.
##

proc dIBuilderCreateEnumerator*(builder: DIBuilderRef; name: cstring; nameLen: csize;
                               value: int64T; isUnsigned: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateEnumerator", dynlib: LLVMLib.}
## *
##  Create debugging information entry for an enumeration.
##  \param Builder        The DIBuilder.
##  \param Scope          Scope in which this enumeration is defined.
##  \param Name           Enumeration name.
##  \param NameLen        Length of enumeration name.
##  \param File           File where this member is defined.
##  \param LineNumber     Line number.
##  \param SizeInBits     Member size.
##  \param AlignInBits    Member alignment.
##  \param Elements       Enumeration elements.
##  \param NumElements    Number of enumeration elements.
##  \param ClassTy        Underlying type of a C++11/ObjC fixed enum.
##

proc dIBuilderCreateEnumerationType*(builder: DIBuilderRef; scope: MetadataRef;
                                    name: cstring; nameLen: csize;
                                    file: MetadataRef; lineNumber: cuint;
                                    sizeInBits: uint64; alignInBits: uint32;
                                    elements: ptr MetadataRef; numElements: cuint;
                                    classTy: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateEnumerationType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a union.
##  \param Builder      The DIBuilder.
##  \param Scope        Scope in which this union is defined.
##  \param Name         Union name.
##  \param NameLen      Length of union name.
##  \param File         File where this member is defined.
##  \param LineNumber   Line number.
##  \param SizeInBits   Member size.
##  \param AlignInBits  Member alignment.
##  \param Flags        Flags to encode member attribute, e.g. private
##  \param Elements     Union elements.
##  \param NumElements  Number of union elements.
##  \param RunTimeLang  Optional parameter, Objective-C runtime version.
##  \param UniqueId     A unique identifier for the union.
##  \param UniqueIdLen  Length of unique identifier.
##

proc dIBuilderCreateUnionType*(builder: DIBuilderRef; scope: MetadataRef;
                              name: cstring; nameLen: csize; file: MetadataRef;
                              lineNumber: cuint; sizeInBits: uint64;
                              alignInBits: uint32; flags: DIFlags;
                              elements: ptr MetadataRef; numElements: cuint;
                              runTimeLang: cuint; uniqueId: cstring;
                              uniqueIdLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateUnionType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for an array.
##  \param Builder      The DIBuilder.
##  \param Size         Array size.
##  \param AlignInBits  Alignment.
##  \param Ty           Element type.
##  \param Subscripts   Subscripts.
##  \param NumSubscripts Number of subscripts.
##

proc dIBuilderCreateArrayType*(builder: DIBuilderRef; size: uint64;
                              alignInBits: uint32; ty: MetadataRef;
                              subscripts: ptr MetadataRef; numSubscripts: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateArrayType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a vector type.
##  \param Builder      The DIBuilder.
##  \param Size         Vector size.
##  \param AlignInBits  Alignment.
##  \param Ty           Element type.
##  \param Subscripts   Subscripts.
##  \param NumSubscripts Number of subscripts.
##

proc dIBuilderCreateVectorType*(builder: DIBuilderRef; size: uint64;
                               alignInBits: uint32; ty: MetadataRef;
                               subscripts: ptr MetadataRef; numSubscripts: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateVectorType", dynlib: LLVMLib.}
## *
##  Create a DWARF unspecified type.
##  \param Builder   The DIBuilder.
##  \param Name      The unspecified type's name.
##  \param NameLen   Length of type name.
##

proc dIBuilderCreateUnspecifiedType*(builder: DIBuilderRef; name: cstring;
                                    nameLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateUnspecifiedType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a basic
##  type.
##  \param Builder     The DIBuilder.
##  \param Name        Type name.
##  \param NameLen     Length of type name.
##  \param SizeInBits  Size of the type.
##  \param Encoding    DWARF encoding code, e.g. \c LLVMDWARFTypeEncoding_float.
##  \param Flags       Flags to encode optional attribute like endianity
##

proc dIBuilderCreateBasicType*(builder: DIBuilderRef; name: cstring; nameLen: csize;
                              sizeInBits: uint64; encoding: DWARFTypeEncoding;
                              flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateBasicType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a pointer.
##  \param Builder     The DIBuilder.
##  \param PointeeTy         Type pointed by this pointer.
##  \param SizeInBits        Size.
##  \param AlignInBits       Alignment. (optional, pass 0 to ignore)
##  \param AddressSpace      DWARF address space. (optional, pass 0 to ignore)
##  \param Name              Pointer type name. (optional)
##  \param NameLen           Length of pointer type name. (optional)
##

proc dIBuilderCreatePointerType*(builder: DIBuilderRef; pointeeTy: MetadataRef;
                                sizeInBits: uint64; alignInBits: uint32;
                                addressSpace: cuint; name: cstring; nameLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreatePointerType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a struct.
##  \param Builder     The DIBuilder.
##  \param Scope        Scope in which this struct is defined.
##  \param Name         Struct name.
##  \param NameLen      Struct name length.
##  \param File         File where this member is defined.
##  \param LineNumber   Line number.
##  \param SizeInBits   Member size.
##  \param AlignInBits  Member alignment.
##  \param Flags        Flags to encode member attribute, e.g. private
##  \param Elements     Struct elements.
##  \param NumElements  Number of struct elements.
##  \param RunTimeLang  Optional parameter, Objective-C runtime version.
##  \param VTableHolder The object containing the vtable for the struct.
##  \param UniqueId     A unique identifier for the struct.
##  \param UniqueIdLen  Length of the unique identifier for the struct.
##

proc dIBuilderCreateStructType*(builder: DIBuilderRef; scope: MetadataRef;
                               name: cstring; nameLen: csize; file: MetadataRef;
                               lineNumber: cuint; sizeInBits: uint64;
                               alignInBits: uint32; flags: DIFlags;
                               derivedFrom: MetadataRef;
                               elements: ptr MetadataRef; numElements: cuint;
                               runTimeLang: cuint; vTableHolder: MetadataRef;
                               uniqueId: cstring; uniqueIdLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateStructType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a member.
##  \param Builder      The DIBuilder.
##  \param Scope        Member scope.
##  \param Name         Member name.
##  \param NameLen      Length of member name.
##  \param File         File where this member is defined.
##  \param LineNo       Line number.
##  \param SizeInBits   Member size.
##  \param AlignInBits  Member alignment.
##  \param OffsetInBits Member offset.
##  \param Flags        Flags to encode member attribute, e.g. private
##  \param Ty           Parent type.
##

proc dIBuilderCreateMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                               name: cstring; nameLen: csize; file: MetadataRef;
                               lineNo: cuint; sizeInBits: uint64;
                               alignInBits: uint32; offsetInBits: uint64;
                               flags: DIFlags; ty: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateMemberType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a
##  C++ static data member.
##  \param Builder      The DIBuilder.
##  \param Scope        Member scope.
##  \param Name         Member name.
##  \param NameLen      Length of member name.
##  \param File         File where this member is declared.
##  \param LineNumber   Line number.
##  \param Type         Type of the static member.
##  \param Flags        Flags to encode member attribute, e.g. private.
##  \param ConstantVal  Const initializer of the member.
##  \param AlignInBits  Member alignment.
##

proc dIBuilderCreateStaticMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                                     name: cstring; nameLen: csize;
                                     file: MetadataRef; lineNumber: cuint;
                                     `type`: MetadataRef; flags: DIFlags;
                                     constantVal: ValueRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateStaticMemberType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a pointer to member.
##  \param Builder      The DIBuilder.
##  \param PointeeType  Type pointed to by this pointer.
##  \param ClassType    Type for which this pointer points to members of.
##  \param SizeInBits   Size.
##  \param AlignInBits  Alignment.
##  \param Flags        Flags.
##

proc dIBuilderCreateMemberPointerType*(builder: DIBuilderRef;
                                      pointeeType: MetadataRef;
                                      classType: MetadataRef; sizeInBits: uint64;
                                      alignInBits: uint32; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateMemberPointerType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for Objective-C instance variable.
##  \param Builder      The DIBuilder.
##  \param Name         Member name.
##  \param NameLen      The length of the C string passed to \c Name.
##  \param File         File where this member is defined.
##  \param LineNo       Line number.
##  \param SizeInBits   Member size.
##  \param AlignInBits  Member alignment.
##  \param OffsetInBits Member offset.
##  \param Flags        Flags to encode member attribute, e.g. private
##  \param Ty           Parent type.
##  \param PropertyNode Property associated with this ivar.
##

proc dIBuilderCreateObjCIVar*(builder: DIBuilderRef; name: cstring; nameLen: csize;
                             file: MetadataRef; lineNo: cuint; sizeInBits: uint64;
                             alignInBits: uint32; offsetInBits: uint64;
                             flags: DIFlags; ty: MetadataRef;
                             propertyNode: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjCIVar", dynlib: LLVMLib.}
## *
##  Create debugging information entry for Objective-C property.
##  \param Builder            The DIBuilder.
##  \param Name               Property name.
##  \param NameLen            The length of the C string passed to \c Name.
##  \param File               File where this property is defined.
##  \param LineNo             Line number.
##  \param GetterName         Name of the Objective C property getter selector.
##  \param GetterNameLen      The length of the C string passed to \c GetterName.
##  \param SetterName         Name of the Objective C property setter selector.
##  \param SetterNameLen      The length of the C string passed to \c SetterName.
##  \param PropertyAttributes Objective C property attributes.
##  \param Ty                 Type.
##

proc dIBuilderCreateObjCProperty*(builder: DIBuilderRef; name: cstring;
                                 nameLen: csize; file: MetadataRef; lineNo: cuint;
                                 getterName: cstring; getterNameLen: csize;
                                 setterName: cstring; setterNameLen: csize;
                                 propertyAttributes: cuint; ty: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjCProperty", dynlib: LLVMLib.}
## *
##  Create a uniqued DIType* clone with FlagObjectPointer and FlagArtificial set.
##  \param Builder   The DIBuilder.
##  \param Type      The underlying type to which this pointer points.
##

proc dIBuilderCreateObjectPointerType*(builder: DIBuilderRef; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjectPointerType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a qualified
##  type, e.g. 'const int'.
##  \param Builder     The DIBuilder.
##  \param Tag         Tag identifying type,
##                     e.g. LLVMDWARFTypeQualifier_volatile_type
##  \param Type        Base Type.
##

proc dIBuilderCreateQualifiedType*(builder: DIBuilderRef; tag: cuint;
                                  `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateQualifiedType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a c++
##  style reference or rvalue reference type.
##  \param Builder   The DIBuilder.
##  \param Tag       Tag identifying type,
##  \param Type      Base Type.
##

proc dIBuilderCreateReferenceType*(builder: DIBuilderRef; tag: cuint;
                                  `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateReferenceType", dynlib: LLVMLib.}
## *
##  Create C++11 nullptr type.
##  \param Builder   The DIBuilder.
##

proc dIBuilderCreateNullPtrType*(builder: DIBuilderRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateNullPtrType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a typedef.
##  \param Builder    The DIBuilder.
##  \param Type       Original type.
##  \param Name       Typedef name.
##  \param File       File where this type is defined.
##  \param LineNo     Line number.
##  \param Scope      The surrounding context for the typedef.
##

proc dIBuilderCreateTypedef*(builder: DIBuilderRef; `type`: MetadataRef;
                            name: cstring; nameLen: csize; file: MetadataRef;
                            lineNo: cuint; scope: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateTypedef", dynlib: LLVMLib.}
## *
##  Create debugging information entry to establish inheritance relationship
##  between two types.
##  \param Builder       The DIBuilder.
##  \param Ty            Original type.
##  \param BaseTy        Base type. Ty is inherits from base.
##  \param BaseOffset    Base offset.
##  \param VBPtrOffset  Virtual base pointer offset.
##  \param Flags         Flags to describe inheritance attribute, e.g. private
##

proc dIBuilderCreateInheritance*(builder: DIBuilderRef; ty: MetadataRef;
                                baseTy: MetadataRef; baseOffset: uint64;
                                vBPtrOffset: uint32; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateInheritance", dynlib: LLVMLib.}
## *
##  Create a permanent forward-declared type.
##  \param Builder             The DIBuilder.
##  \param Tag                 A unique tag for this type.
##  \param Name                Type name.
##  \param NameLen             Length of type name.
##  \param Scope               Type scope.
##  \param File                File where this type is defined.
##  \param Line                Line number where this type is defined.
##  \param RuntimeLang         Indicates runtime version for languages like
##                             Objective-C.
##  \param SizeInBits          Member size.
##  \param AlignInBits         Member alignment.
##  \param UniqueIdentifier    A unique identifier for the type.
##  \param UniqueIdentifierLen Length of the unique identifier.
##

proc dIBuilderCreateForwardDecl*(builder: DIBuilderRef; tag: cuint; name: cstring;
                                nameLen: csize; scope: MetadataRef;
                                file: MetadataRef; line: cuint; runtimeLang: cuint;
                                sizeInBits: uint64; alignInBits: uint32;
                                uniqueIdentifier: cstring;
                                uniqueIdentifierLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateForwardDecl", dynlib: LLVMLib.}
## *
##  Create a temporary forward-declared type.
##  \param Builder             The DIBuilder.
##  \param Tag                 A unique tag for this type.
##  \param Name                Type name.
##  \param NameLen             Length of type name.
##  \param Scope               Type scope.
##  \param File                File where this type is defined.
##  \param Line                Line number where this type is defined.
##  \param RuntimeLang         Indicates runtime version for languages like
##                             Objective-C.
##  \param SizeInBits          Member size.
##  \param AlignInBits         Member alignment.
##  \param Flags               Flags.
##  \param UniqueIdentifier    A unique identifier for the type.
##  \param UniqueIdentifierLen Length of the unique identifier.
##

proc dIBuilderCreateReplaceableCompositeType*(builder: DIBuilderRef; tag: cuint;
    name: cstring; nameLen: csize; scope: MetadataRef; file: MetadataRef; line: cuint;
    runtimeLang: cuint; sizeInBits: uint64; alignInBits: uint32; flags: DIFlags;
    uniqueIdentifier: cstring; uniqueIdentifierLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateReplaceableCompositeType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a bit field member.
##  \param Builder             The DIBuilder.
##  \param Scope               Member scope.
##  \param Name                Member name.
##  \param NameLen             Length of member name.
##  \param File                File where this member is defined.
##  \param LineNumber          Line number.
##  \param SizeInBits          Member size.
##  \param OffsetInBits        Member offset.
##  \param StorageOffsetInBits Member storage offset.
##  \param Flags               Flags to encode member attribute.
##  \param Type                Parent type.
##

proc dIBuilderCreateBitFieldMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                                       name: cstring; nameLen: csize;
                                       file: MetadataRef; lineNumber: cuint;
                                       sizeInBits: uint64; offsetInBits: uint64T;
                                       storageOffsetInBits: uint64;
                                       flags: DIFlags; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateBitFieldMemberType", dynlib: LLVMLib.}
## *
##  Create debugging information entry for a class.
##  \param Scope               Scope in which this class is defined.
##  \param Name                Class name.
##  \param NameLen             The length of the C string passed to \c Name.
##  \param File                File where this member is defined.
##  \param LineNumber          Line number.
##  \param SizeInBits          Member size.
##  \param AlignInBits         Member alignment.
##  \param OffsetInBits        Member offset.
##  \param Flags               Flags to encode member attribute, e.g. private.
##  \param DerivedFrom         Debug info of the base class of this type.
##  \param Elements            Class members.
##  \param NumElements         Number of class elements.
##  \param VTableHolder        Debug info of the base class that contains vtable
##                             for this type. This is used in
##                             DW_AT_containing_type. See DWARF documentation
##                             for more info.
##  \param TemplateParamsNode  Template type parameters.
##  \param UniqueIdentifier    A unique identifier for the type.
##  \param UniqueIdentifierLen Length of the unique identifier.
##

proc dIBuilderCreateClassType*(builder: DIBuilderRef; scope: MetadataRef;
                              name: cstring; nameLen: csize; file: MetadataRef;
                              lineNumber: cuint; sizeInBits: uint64;
                              alignInBits: uint32; offsetInBits: uint64;
                              flags: DIFlags; derivedFrom: MetadataRef;
                              elements: ptr MetadataRef; numElements: cuint;
                              vTableHolder: MetadataRef;
                              templateParamsNode: MetadataRef;
                              uniqueIdentifier: cstring;
                              uniqueIdentifierLen: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateClassType", dynlib: LLVMLib.}
## *
##  Create a uniqued DIType* clone with FlagArtificial set.
##  \param Builder     The DIBuilder.
##  \param Type        The underlying type.
##

proc dIBuilderCreateArtificialType*(builder: DIBuilderRef; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateArtificialType", dynlib: LLVMLib.}
## *
##  Get the name of this DIType.
##  \param DType     The DIType.
##  \param Length    The length of the returned string.
##
##  @see DIType::getName()
##

proc dITypeGetName*(dType: MetadataRef; length: ptr csize): cstring {.
    importc: "LLVMDITypeGetName", dynlib: LLVMLib.}
## *
##  Get the size of this DIType in bits.
##  \param DType     The DIType.
##
##  @see DIType::getSizeInBits()
##

proc dITypeGetSizeInBits*(dType: MetadataRef): uint64 {.
    importc: "LLVMDITypeGetSizeInBits", dynlib: LLVMLib.}
## *
##  Get the offset of this DIType in bits.
##  \param DType     The DIType.
##
##  @see DIType::getOffsetInBits()
##

proc dITypeGetOffsetInBits*(dType: MetadataRef): uint64 {.
    importc: "LLVMDITypeGetOffsetInBits", dynlib: LLVMLib.}
## *
##  Get the alignment of this DIType in bits.
##  \param DType     The DIType.
##
##  @see DIType::getAlignInBits()
##

proc dITypeGetAlignInBits*(dType: MetadataRef): uint32 {.
    importc: "LLVMDITypeGetAlignInBits", dynlib: LLVMLib.}
## *
##  Get the source line where this DIType is declared.
##  \param DType     The DIType.
##
##  @see DIType::getLine()
##

proc dITypeGetLine*(dType: MetadataRef): cuint {.importc: "LLVMDITypeGetLine",
    dynlib: LLVMLib.}
## *
##  Get the flags associated with this DIType.
##  \param DType     The DIType.
##
##  @see DIType::getFlags()
##

proc dITypeGetFlags*(dType: MetadataRef): DIFlags {.importc: "LLVMDITypeGetFlags",
    dynlib: LLVMLib.}
## *
##  Create a descriptor for a value range.
##  \param Builder    The DIBuilder.
##  \param LowerBound Lower bound of the subrange, e.g. 0 for C, 1 for Fortran.
##  \param Count      Count of elements in the subrange.
##

proc dIBuilderGetOrCreateSubrange*(builder: DIBuilderRef; lowerBound: int64T;
                                  count: int64T): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateSubrange", dynlib: LLVMLib.}
## *
##  Create an array of DI Nodes.
##  \param Builder        The DIBuilder.
##  \param Data           The DI Node elements.
##  \param NumElements    Number of DI Node elements.
##

proc dIBuilderGetOrCreateArray*(builder: DIBuilderRef; data: ptr MetadataRef;
                               numElements: csize): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateArray", dynlib: LLVMLib.}
## *
##  Create a new descriptor for the specified variable which has a complex
##  address expression for its address.
##  \param Builder     The DIBuilder.
##  \param Addr        An array of complex address operations.
##  \param Length      Length of the address operation array.
##

proc dIBuilderCreateExpression*(builder: DIBuilderRef; `addr`: ptr int64T;
                               length: csize): MetadataRef {.
    importc: "LLVMDIBuilderCreateExpression", dynlib: LLVMLib.}
## *
##  Create a new descriptor for the specified variable that does not have an
##  address, but does have a constant value.
##  \param Builder     The DIBuilder.
##  \param Value       The constant value.
##

proc dIBuilderCreateConstantValueExpression*(builder: DIBuilderRef; value: int64T): MetadataRef {.
    importc: "LLVMDIBuilderCreateConstantValueExpression", dynlib: LLVMLib.}
## *
##  Create a new descriptor for the specified variable.
##  \param Scope       Variable scope.
##  \param Name        Name of the variable.
##  \param NameLen     The length of the C string passed to \c Name.
##  \param Linkage     Mangled  name of the variable.
##  \param LinkLen     The length of the C string passed to \c Linkage.
##  \param File        File where this variable is defined.
##  \param LineNo      Line number.
##  \param Ty          Variable Type.
##  \param LocalToUnit Boolean flag indicate whether this variable is
##                     externally visible or not.
##  \param Expr        The location of the global relative to the attached
##                     GlobalVariable.
##  \param Decl        Reference to the corresponding declaration.
##                     variables.
##  \param AlignInBits Variable alignment(or 0 if no alignment attr was
##                     specified)
##

proc dIBuilderCreateGlobalVariableExpression*(builder: DIBuilderRef;
    scope: MetadataRef; name: cstring; nameLen: csize; linkage: cstring; linkLen: csize;
    file: MetadataRef; lineNo: cuint; ty: MetadataRef; localToUnit: Bool;
    expr: MetadataRef; decl: MetadataRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateGlobalVariableExpression", dynlib: LLVMLib.}
## *
##  Retrieves the \c DIVariable associated with this global variable expression.
##  \param GVE    The global variable expression.
##
##  @see llvm::DIGlobalVariableExpression::getVariable()
##

proc dIGlobalVariableExpressionGetVariable*(gve: MetadataRef): MetadataRef {.
    importc: "LLVMDIGlobalVariableExpressionGetVariable", dynlib: LLVMLib.}
## *
##  Retrieves the \c DIExpression associated with this global variable expression.
##  \param GVE    The global variable expression.
##
##  @see llvm::DIGlobalVariableExpression::getExpression()
##

proc dIGlobalVariableExpressionGetExpression*(gve: MetadataRef): MetadataRef {.
    importc: "LLVMDIGlobalVariableExpressionGetExpression", dynlib: LLVMLib.}
## *
##  Get the metadata of the file associated with a given variable.
##  \param Var     The variable object.
##
##  @see DIVariable::getFile()
##

proc dIVariableGetFile*(`var`: MetadataRef): MetadataRef {.
    importc: "LLVMDIVariableGetFile", dynlib: LLVMLib.}
## *
##  Get the metadata of the scope associated with a given variable.
##  \param Var     The variable object.
##
##  @see DIVariable::getScope()
##

proc dIVariableGetScope*(`var`: MetadataRef): MetadataRef {.
    importc: "LLVMDIVariableGetScope", dynlib: LLVMLib.}
## *
##  Get the source line where this \c DIVariable is declared.
##  \param Var     The DIVariable.
##
##  @see DIVariable::getLine()
##

proc dIVariableGetLine*(`var`: MetadataRef): cuint {.
    importc: "LLVMDIVariableGetLine", dynlib: LLVMLib.}
## *
##  Create a new temporary \c MDNode.  Suitable for use in constructing cyclic
##  \c MDNode structures. A temporary \c MDNode is not uniqued, may be RAUW'd,
##  and must be manually deleted with \c LLVMDisposeTemporaryMDNode.
##  \param Ctx            The context in which to construct the temporary node.
##  \param Data           The metadata elements.
##  \param NumElements    Number of metadata elements.
##

proc temporaryMDNode*(ctx: ContextRef; data: ptr MetadataRef; numElements: csize): MetadataRef {.
    importc: "LLVMTemporaryMDNode", dynlib: LLVMLib.}
## *
##  Deallocate a temporary node.
##
##  Calls \c replaceAllUsesWith(nullptr) before deleting, so any remaining
##  references will be reset.
##  \param TempNode    The temporary metadata node.
##

proc disposeTemporaryMDNode*(tempNode: MetadataRef) {.
    importc: "LLVMDisposeTemporaryMDNode", dynlib: LLVMLib.}
## *
##  Replace all uses of temporary metadata.
##  \param TempTargetMetadata    The temporary metadata node.
##  \param Replacement           The replacement metadata node.
##

proc metadataReplaceAllUsesWith*(tempTargetMetadata: MetadataRef;
                                replacement: MetadataRef) {.
    importc: "LLVMMetadataReplaceAllUsesWith", dynlib: LLVMLib.}
## *
##  Create a new descriptor for the specified global variable that is temporary
##  and meant to be RAUWed.
##  \param Scope       Variable scope.
##  \param Name        Name of the variable.
##  \param NameLen     The length of the C string passed to \c Name.
##  \param Linkage     Mangled  name of the variable.
##  \param LnkLen      The length of the C string passed to \c Linkage.
##  \param File        File where this variable is defined.
##  \param LineNo      Line number.
##  \param Ty          Variable Type.
##  \param LocalToUnit Boolean flag indicate whether this variable is
##                     externally visible or not.
##  \param Decl        Reference to the corresponding declaration.
##  \param AlignInBits Variable alignment(or 0 if no alignment attr was
##                     specified)
##

proc dIBuilderCreateTempGlobalVariableFwdDecl*(builder: DIBuilderRef;
    scope: MetadataRef; name: cstring; nameLen: csize; linkage: cstring; lnkLen: csize;
    file: MetadataRef; lineNo: cuint; ty: MetadataRef; localToUnit: Bool;
    decl: MetadataRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateTempGlobalVariableFwdDecl", dynlib: LLVMLib.}
## *
##  Insert a new llvm.dbg.declare intrinsic call before the given instruction.
##  \param Builder     The DIBuilder.
##  \param Storage     The storage of the variable to declare.
##  \param VarInfo     The variable's debug info descriptor.
##  \param Expr        A complex location expression for the variable.
##  \param DebugLoc    Debug info location.
##  \param Instr       Instruction acting as a location for the new intrinsic.
##

proc dIBuilderInsertDeclareBefore*(builder: DIBuilderRef; storage: ValueRef;
                                  varInfo: MetadataRef; expr: MetadataRef;
                                  debugLoc: MetadataRef; instr: ValueRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDeclareBefore", dynlib: LLVMLib.}
## *
##  Insert a new llvm.dbg.declare intrinsic call at the end of the given basic
##  block. If the basic block has a terminator instruction, the intrinsic is
##  inserted before that terminator instruction.
##  \param Builder     The DIBuilder.
##  \param Storage     The storage of the variable to declare.
##  \param VarInfo     The variable's debug info descriptor.
##  \param Expr        A complex location expression for the variable.
##  \param DebugLoc    Debug info location.
##  \param Block       Basic block acting as a location for the new intrinsic.
##

proc dIBuilderInsertDeclareAtEnd*(builder: DIBuilderRef; storage: ValueRef;
                                 varInfo: MetadataRef; expr: MetadataRef;
                                 debugLoc: MetadataRef; `block`: BasicBlockRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDeclareAtEnd", dynlib: LLVMLib.}
## *
##  Insert a new llvm.dbg.value intrinsic call before the given instruction.
##  \param Builder     The DIBuilder.
##  \param Val         The value of the variable.
##  \param VarInfo     The variable's debug info descriptor.
##  \param Expr        A complex location expression for the variable.
##  \param DebugLoc    Debug info location.
##  \param Instr       Instruction acting as a location for the new intrinsic.
##

proc dIBuilderInsertDbgValueBefore*(builder: DIBuilderRef; val: ValueRef;
                                   varInfo: MetadataRef; expr: MetadataRef;
                                   debugLoc: MetadataRef; instr: ValueRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDbgValueBefore", dynlib: LLVMLib.}
## *
##  Insert a new llvm.dbg.value intrinsic call at the end of the given basic
##  block. If the basic block has a terminator instruction, the intrinsic is
##  inserted before that terminator instruction.
##  \param Builder     The DIBuilder.
##  \param Val         The value of the variable.
##  \param VarInfo     The variable's debug info descriptor.
##  \param Expr        A complex location expression for the variable.
##  \param DebugLoc    Debug info location.
##  \param Block       Basic block acting as a location for the new intrinsic.
##

proc dIBuilderInsertDbgValueAtEnd*(builder: DIBuilderRef; val: ValueRef;
                                  varInfo: MetadataRef; expr: MetadataRef;
                                  debugLoc: MetadataRef; `block`: BasicBlockRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDbgValueAtEnd", dynlib: LLVMLib.}
## *
##  Create a new descriptor for a local auto variable.
##  \param Builder         The DIBuilder.
##  \param Scope           The local scope the variable is declared in.
##  \param Name            Variable name.
##  \param NameLen         Length of variable name.
##  \param File            File where this variable is defined.
##  \param LineNo          Line number.
##  \param Ty              Metadata describing the type of the variable.
##  \param AlwaysPreserve  If true, this descriptor will survive optimizations.
##  \param Flags           Flags.
##  \param AlignInBits     Variable alignment.
##

proc dIBuilderCreateAutoVariable*(builder: DIBuilderRef; scope: MetadataRef;
                                 name: cstring; nameLen: csize; file: MetadataRef;
                                 lineNo: cuint; ty: MetadataRef;
                                 alwaysPreserve: Bool; flags: DIFlags;
                                 alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateAutoVariable", dynlib: LLVMLib.}
## *
##  Create a new descriptor for a function parameter variable.
##  \param Builder         The DIBuilder.
##  \param Scope           The local scope the variable is declared in.
##  \param Name            Variable name.
##  \param NameLen         Length of variable name.
##  \param ArgNo           Unique argument number for this variable; starts at 1.
##  \param File            File where this variable is defined.
##  \param LineNo          Line number.
##  \param Ty              Metadata describing the type of the variable.
##  \param AlwaysPreserve  If true, this descriptor will survive optimizations.
##  \param Flags           Flags.
##

proc dIBuilderCreateParameterVariable*(builder: DIBuilderRef; scope: MetadataRef;
                                      name: cstring; nameLen: csize; argNo: cuint;
                                      file: MetadataRef; lineNo: cuint;
                                      ty: MetadataRef; alwaysPreserve: Bool;
                                      flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateParameterVariable", dynlib: LLVMLib.}
## *
##  Get the metadata of the subprogram attached to a function.
##
##  @see llvm::Function::getSubprogram()
##

proc getSubprogram*(`func`: ValueRef): MetadataRef {.importc: "LLVMGetSubprogram",
    dynlib: LLVMLib.}
## *
##  Set the subprogram attached to a function.
##
##  @see llvm::Function::setSubprogram()
##

proc setSubprogram*(`func`: ValueRef; sp: MetadataRef) {.
    importc: "LLVMSetSubprogram", dynlib: LLVMLib.}
## *
##  Get the line associated with a given subprogram.
##  \param Subprogram     The subprogram object.
##
##  @see DISubprogram::getLine()
##

proc dISubprogramGetLine*(subprogram: MetadataRef): cuint {.
    importc: "LLVMDISubprogramGetLine", dynlib: LLVMLib.}
## *
##  Get the debug location for the given instruction.
##
##  @see llvm::Instruction::getDebugLoc()
##

proc instructionGetDebugLoc*(inst: ValueRef): MetadataRef {.
    importc: "LLVMInstructionGetDebugLoc", dynlib: LLVMLib.}
## *
##  Set the debug location for the given instruction.
##
##  To clear the location metadata of the given instruction, pass NULL to \p Loc.
##
##  @see llvm::Instruction::setDebugLoc()
##

proc instructionSetDebugLoc*(inst: ValueRef; loc: MetadataRef) {.
    importc: "LLVMInstructionSetDebugLoc", dynlib: LLVMLib.}
## *
##  Obtain the enumerated type of a Metadata instance.
##
##  @see llvm::Metadata::getMetadataID()
##

proc getMetadataKind*(metadata: MetadataRef): MetadataKind {.
    importc: "LLVMGetMetadataKind", dynlib: LLVMLib.}