#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <DbgHelp.h>
#include <stdio.h>

// http://www.cnblogs.com/drunkard87/p/3509493.html
// http://jpassing.com/2009/04/22/uniquely-identifying-a-modules-build/
typedef struct _CV_INFO_PDB70
{
  ULONG CvSignature;
  GUID Guid;
  ULONG Age;
  UCHAR PdbFileName[0];
} CV_INFO_PDB70, *PCV_INFO_PDB70;

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage:\n");
		printf("  DumpInfo.exe <dmpfile>\n");
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


	PMINIDUMP_HEADER pMiniHeader = NULL;
	PMINIDUMP_DIRECTORY pMiniDirectory = NULL;

	pMiniHeader = (PMINIDUMP_HEADER)pFilePointer;
	if (pMiniHeader->Signature != MINIDUMP_SIGNATURE)
	{
		printf("%s: not a .dmp file\n", argv[1]);
		goto OnError;
	}

	pMiniDirectory = (PMINIDUMP_DIRECTORY)((intptr_t)pFilePointer + pMiniHeader->StreamDirectoryRva);

	for (ULONG i=0; i<pMiniHeader->NumberOfStreams; ++i)
	{
		// find exe/dll module
		if (pMiniDirectory->StreamType == ModuleListStream)
		{
			PMINIDUMP_MODULE_LIST pModuleList = (PMINIDUMP_MODULE_LIST)((intptr_t)pFilePointer + pMiniDirectory->Location.Rva);

			for (unsigned int j = 0; j < pModuleList->NumberOfModules; ++j)
			{
				PMINIDUMP_MODULE pModule = &pModuleList->Modules[j];
				PMINIDUMP_STRING pString = (PMINIDUMP_STRING)((intptr_t)pFilePointer + pModule->ModuleNameRva);

				printf("Name: RVA=0x%x, Length=%u - %ws\n", pModule->ModuleNameRva, pString->Length, pString->Buffer);
				printf("BaseOfImage: 0x%I64x\n", pModule->BaseOfImage);
				printf("SizeOfImage: %u\n", pModule->SizeOfImage);
				printf("CheckSum: %u\n", pModule->CheckSum);
				printf("Stamp: %u\n", pModule->TimeDateStamp);
				printf("CvRecord: DataSize=%u, Rva=%u\n", pModule->CvRecord.DataSize, pModule->CvRecord.Rva);

				if (pModule->CvRecord.Rva != 0)
				{
					PCV_INFO_PDB70 pCvInfo = (PCV_INFO_PDB70)((intptr_t)pFilePointer + pModule->CvRecord.Rva);
					printf("  Age: %04X\n", pCvInfo->Age);
					printf("  CvSignature: %04X\n", pCvInfo->CvSignature);
					printf("  PdbFileName: %s\n", pCvInfo->PdbFileName);
					printf("  Guid : %08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X\n",
						pCvInfo->Guid.Data1, pCvInfo->Guid.Data2, pCvInfo->Guid.Data3,
						pCvInfo->Guid.Data4[0], pCvInfo->Guid.Data4[1], pCvInfo->Guid.Data4[2], 
						pCvInfo->Guid.Data4[3], pCvInfo->Guid.Data4[4], pCvInfo->Guid.Data4[5],
						pCvInfo->Guid.Data4[6], pCvInfo->Guid.Data4[7]);
				}

				printf("\n");
			}
		}

		// printf("ModuleType: %u\n", pMiniDirectory->StreamType);

		++pMiniDirectory;
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