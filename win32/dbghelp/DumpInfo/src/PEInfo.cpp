#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <DbgHelp.h>
#include <stdio.h>
#include <assert.h>

// http://www.cnblogs.com/drunkard87/p/3509493.html
// http://jpassing.com/2009/04/22/uniquely-identifying-a-modules-build/
typedef struct _CV_INFO_PDB70
{
  ULONG CvSignature;
  GUID Guid;
  ULONG Age;
  UCHAR PdbFileName[0];
} CV_INFO_PDB70, *PCV_INFO_PDB70;

#define PtrFromOffset( base, offset ) ( ( ( PUCHAR ) base ) + offset )

static PIMAGE_DATA_DIRECTORY GetDebugDataDirectory(
	__in PVOID LoadAddress
)
{
	PIMAGE_DOS_HEADER DosHeader = ( PIMAGE_DOS_HEADER ) ( PVOID ) LoadAddress;
	PIMAGE_NT_HEADERS NtHeader = ( PIMAGE_NT_HEADERS ) PtrFromOffset( DosHeader, DosHeader->e_lfanew );
	assert( IMAGE_NT_SIGNATURE == NtHeader->Signature );

	return &NtHeader->OptionalHeader.DataDirectory[ IMAGE_DIRECTORY_ENTRY_DEBUG ];
}

static ULONG RvaToOffset(PVOID LoadAddress, DWORD Rva)
{
	PIMAGE_DOS_HEADER DosHeader = ( PIMAGE_DOS_HEADER ) ( PVOID ) LoadAddress;
	PIMAGE_NT_HEADERS NtHeader = ( PIMAGE_NT_HEADERS ) PtrFromOffset( DosHeader, DosHeader->e_lfanew );
	assert( IMAGE_NT_SIGNATURE == NtHeader->Signature );

	PIMAGE_SECTION_HEADER SectionHeader = (PIMAGE_SECTION_HEADER) PtrFromOffset( NtHeader, sizeof(IMAGE_NT_HEADERS) );
	for (WORD i = 0; i < NtHeader->FileHeader.NumberOfSections; ++i)
	{
		if (SectionHeader->VirtualAddress <= Rva && Rva < SectionHeader->VirtualAddress+SectionHeader->SizeOfRawData)
		{
			return SectionHeader->PointerToRawData + Rva - SectionHeader->VirtualAddress;
		}

		++SectionHeader;
	}

	assert(0 && "shouldn't be here");
	return 0;
}

#if 0
NTSTATUS GetDebugGuid(
  __in ULONG_PTR ModuleBaseAddress,
  __out GUID *Guid
  )
{
  PIMAGE_DATA_DIRECTORY DebugDataDirectory;
  PIMAGE_DEBUG_DIRECTORY DebugHeaders;
  ULONG Index;
  ULONG NumberOfDebugDirs;
  ULONG_PTR ModuleBaseAddress;
  NTSTATUS Status;

  DebugDataDirectory  = DebugDataDirectory( ModuleBaseAddress );
  DebugHeaders    = ( PIMAGE_DEBUG_DIRECTORY ) PtrFromRva( 
    ModuleBaseAddress, 
    DebugDataDirectory->VirtualAddress );

  ASSERT( ( DebugDataDirectory->Size % sizeof( IMAGE_DEBUG_DIRECTORY ) ) == 0 );
  NumberOfDebugDirs = DebugDataDirectory->Size / sizeof( IMAGE_DEBUG_DIRECTORY );

  //
  // Lookup CodeView record.
  //
  for ( Index = 0; Index < NumberOfDebugDirs; Index++ )
  {
    PCV_INFO_PDB70 CvInfo;
    if ( DebugHeaders[ Index ].Type != IMAGE_DEBUG_TYPE_CODEVIEW )
    {
      continue;
    }

    CvInfo = ( PCV_INFO_PDB70 ) PtrFromRva( 
      ModuleBaseAddress, 
      DebugHeaders[ Index ].AddressOfRawData );

    if ( CvInfo->CvSignature != 'SDSR' )
    {
      //
      // Weird, old PDB format maybe.
      //
      return STATUS_xxx_UNRECOGNIZED_CV_HEADER;
    }

    *Guid = CvInfo->Signature;
    return STATUS_SUCCESS;  
  }

  return STATUS_xxx_CV_GUID_LOOKUP_FAILED;
}
#endif


int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage:\n");
		printf("  PEInfo.exe <exe/dll>\n");
		return -1;
	}

	HANDLE hFile = NULL;
	HANDLE hMemMap = NULL;
	PVOID pFilePointer = NULL;

	hFile = CreateFile(argv[1], GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		printf("Can't open file: %s\n", argv[1]);
		goto OnError;
	}

	hMemMap = CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL);
	if (hMemMap == NULL)
	{
		printf("CreateFileMapping() failed\n");
		goto OnError;
	}

	pFilePointer = MapViewOfFile(hMemMap, FILE_MAP_READ, 0, 0, 0);
	if (pFilePointer == NULL)
	{
		printf("MapViewOfFile() failed\n");
		goto OnError;
	}


	PIMAGE_DATA_DIRECTORY DebugDataDirectory;
	PIMAGE_DEBUG_DIRECTORY DebugHeaders;
	ULONG Index;
	ULONG NumberOfDebugDirs;

	DebugDataDirectory = GetDebugDataDirectory( pFilePointer );
	DebugHeaders = (PIMAGE_DEBUG_DIRECTORY) PtrFromOffset(pFilePointer, RvaToOffset(pFilePointer, DebugDataDirectory->VirtualAddress));

	assert( ( DebugDataDirectory->Size % sizeof( IMAGE_DEBUG_DIRECTORY ) ) == 0 );
	NumberOfDebugDirs = DebugDataDirectory->Size / sizeof( IMAGE_DEBUG_DIRECTORY );

	//
	// Lookup CodeView record.
	//
	for ( Index = 0; Index < NumberOfDebugDirs; Index++ )
	{
		PCV_INFO_PDB70 CvInfo;
		if ( DebugHeaders[Index].Type != IMAGE_DEBUG_TYPE_CODEVIEW )
		{
			continue;
		}

		CvInfo = (PCV_INFO_PDB70) PtrFromOffset(pFilePointer, DebugHeaders[Index].PointerToRawData);

		if ( CvInfo->CvSignature != 'SDSR' )
		{
			//
			// Weird, old PDB format maybe.
			//
			// return STATUS_xxx_UNRECOGNIZED_CV_HEADER;
			continue;
		}

		printf("  Age: %04X\n", CvInfo->Age);
		printf("  CvSignature: %04X\n", CvInfo->CvSignature);
		printf("  PdbFileName: %s\n", CvInfo->PdbFileName);
		printf("  Guid : %08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X\n",
			CvInfo->Guid.Data1, CvInfo->Guid.Data2, CvInfo->Guid.Data3,
			CvInfo->Guid.Data4[0], CvInfo->Guid.Data4[1], CvInfo->Guid.Data4[2], 
			CvInfo->Guid.Data4[3], CvInfo->Guid.Data4[4], CvInfo->Guid.Data4[5],
			CvInfo->Guid.Data4[6], CvInfo->Guid.Data4[7]);
	}


OnError:
	if (pFilePointer)
	{
		UnmapViewOfFile(pFilePointer);
		pFilePointer = NULL;
	}

	if (hMemMap != INVALID_HANDLE_VALUE)
	{
		CloseHandle(hMemMap);
		hMemMap = NULL;
	}

	if (hFile != INVALID_HANDLE_VALUE)
	{
		CloseHandle(hFile);
		hFile = NULL;
	}

	return 0;
}