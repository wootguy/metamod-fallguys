#include <extdll.h>

#include <dllapi.h>
#include <meta_api.h>

#include <cl_entity.h>
#include <entity_state.h>

#include <vector>

#include "asext.h"

#include "serverdef.h"

PRIVATE_FUNCTION_DEFINE(CASHook_Call);
PRIVATE_FUNCTION_DEFINE(CASHook_CASHook);
PRIVATE_FUNCTION_DEFINE(CASDocumentation_RegisterObjectType);
PRIVATE_FUNCTION_DEFINE(CASDocumentation_RegisterObjectProperty);
PRIVATE_FUNCTION_DEFINE(CASDocumentation_RegisterObjectMethod);
PRIVATE_FUNCTION_DEFINE(CASDocumentation_RegisterObjectBehaviour);
PRIVATE_FUNCTION_DEFINE(CASDocumentation_RegisterFuncDef);
PRIVATE_FUNCTION_DEFINE(CASDirectoryList_CreateDirectory);
PRIVATE_FUNCTION_DEFINE(CASFunction_Create);
PRIVATE_FUNCTION_DEFINE(CASBaseCallable_Call);
PRIVATE_FUNCTION_DEFINE(CASRefCountedBaseClass_InternalRelease);
PRIVATE_FUNCTION_DEFINE(CScriptAny_Release);
PRIVATE_FUNCTION_DEFINE(CString_Assign);
PRIVATE_FUNCTION_DEFINE(CString_dtor);

CASServerManager **g_pServerManager = NULL;

fnASEXT_CallHook ASEXT_CallHook = NULL;

fnASEXT_CallCASBaseCallable ASEXT_CallCASBaseCallable = NULL;

bool g_ASDocInit = false;
std::vector<fnASDocInitCallback> g_ASDocInitCallbacks;

bool g_ASDirInit = false;
std::vector<fnASDirInitCallback> g_ASDirInitCallbacks;

std::vector<CASHook *> g_ASHooks;

C_DLLEXPORT void ASEXT_RegisterObjectMethod(CASDocumentation *pthis, const char *docs, const char *name, const char *func, void *pfn, int type)
{
	SC_SERVER_DUMMYVAR;

	asSFuncPtr reg;
	reg.pfn = pfn;
	reg.flag = 3;//3 = method

	g_call_original_CASDocumentation_RegisterObjectMethod(pthis, SC_SERVER_PASS_DUMMYARG docs, name, func, &reg, type);
}

C_DLLEXPORT void ASEXT_RegisterObjectBehaviour(CASDocumentation *pthis, const char *docs, const char *name, int behaviour, const char *func, void *pfn, int type)
{
	SC_SERVER_DUMMYVAR;

	asSFuncPtr reg;
	reg.pfn = pfn;
	reg.flag = 2;//2 = global func

	g_call_original_CASDocumentation_RegisterObjectBehaviour(pthis, SC_SERVER_PASS_DUMMYARG docs, name, behaviour, func, &reg, type, 0);
}

C_DLLEXPORT void ASEXT_RegisterObjectType(CASDocumentation *pthis, const char *docs, const char *name, int size, unsigned int flags)
{
	SC_SERVER_DUMMYVAR;

	g_call_original_CASDocumentation_RegisterObjectType(pthis, SC_SERVER_PASS_DUMMYARG docs, name, size, flags);
}

C_DLLEXPORT void ASEXT_RegisterObjectProperty(CASDocumentation *pthis, const char *docs, const char *name, const char *prop, int offset)
{
	SC_SERVER_DUMMYVAR;

	g_call_original_CASDocumentation_RegisterObjectProperty(pthis, SC_SERVER_PASS_DUMMYARG docs, name, prop, offset);
}

C_DLLEXPORT void ASEXT_RegisterFuncDef(CASDocumentation *pthis, const char *docs, const char *funcdef)
{
	SC_SERVER_DUMMYVAR;

	g_call_original_CASDocumentation_RegisterFuncDef(pthis, SC_SERVER_PASS_DUMMYARG docs, funcdef);
}

int SC_SERVER_DECL NewCASDocumentation_RegisterObjectType(CASDocumentation *pthis, SC_SERVER_DUMMYARG const char *docs, const char *name, int size, unsigned int flags)
{
	if (name && docs && !strcmp(name, "CSurvivalMode") && !strcmp(docs, "Survival Mode handler") && flags == 0x40001u)
	{
		for (size_t i = 0; i < g_ASDocInitCallbacks.size(); ++i)
		{
			g_ASDocInitCallbacks[i](pthis);
		}

		g_ASDocInitCallbacks.clear();
		g_ASDocInit = true;
	} 

	return g_call_original_CASDocumentation_RegisterObjectType(pthis, SC_SERVER_PASS_DUMMYARG docs, name, size, flags);
}

void SC_SERVER_DECL NewCASDirectoryList_CreateDirectory(CASDirectoryList *pthis, SC_SERVER_DUMMYARG const char *path, unsigned char flags, unsigned char access_control, unsigned char permanent, unsigned char unk)
{
	if (!strcmp(path, "scripts/plugins/store") && flags == ASFlag_Plugin && access_control == (ASFileAccessControl_Read | ASFileAccessControl_Write) && permanent == 1)
	{
		for (size_t i = 0; i < g_ASDirInitCallbacks.size(); ++i)
		{
			g_ASDirInitCallbacks[i](pthis);
		}

		g_ASDirInitCallbacks.clear();
		g_ASDirInit = true;
	}

	g_call_original_CASDirectoryList_CreateDirectory(pthis, SC_SERVER_PASS_DUMMYARG path, flags, access_control, permanent, unk);
}

C_DLLEXPORT void ASEXT_CreateDirectory(void *pthis, const char *path, unsigned char flags, unsigned char access_control, unsigned char permanent, unsigned char unk)
{
	SC_SERVER_DUMMYVAR;
	g_call_original_CASDirectoryList_CreateDirectory((CASDirectoryList *)pthis, SC_SERVER_PASS_DUMMYARG path, flags, access_control, permanent, unk);
}

C_DLLEXPORT bool ASEXT_RegisterDocInitCallback(fnASDocInitCallback callback)
{
	if (g_ASDocInit)
		return false;

	g_ASDocInitCallbacks.emplace_back(callback);

	return true;
}

C_DLLEXPORT bool ASEXT_RegisterDirInitCallback(fnASDirInitCallback callback)
{
	if (g_ASDirInit)
		return false;

	g_ASDirInitCallbacks.emplace_back(callback);

	return true;
}

C_DLLEXPORT void *ASEXT_RegisterHook(const char *docs, int stopMode, int type, int flags, const char *domain, const char *func, const char *args)
{
	SC_SERVER_DUMMYVAR;

	CASHookRegistration reg;
	reg.unk = 0;
	reg.stopMode = stopMode;
	reg.docs = docs;

	CASHook *hook = new CASHook;
	g_ASHooks.emplace_back(hook);

	g_pfn_CASHook_CASHook(hook, SC_SERVER_PASS_DUMMYARG type, flags, domain, func, args, &reg);

	return hook;
}

C_DLLEXPORT void ASEXT_CStringAssign(void *pthis, const char *src, size_t len)
{
	SC_SERVER_DUMMYVAR;

	g_pfn_CString_Assign((CString *)pthis, SC_SERVER_PASS_DUMMYARG src, len);
}

C_DLLEXPORT void ASEXT_CStringdtor(void *pthis)
{
	SC_SERVER_DUMMYVAR;

	g_pfn_CString_dtor((CString *)pthis SC_SERVER_PASS_DUMMYARG2);
}

C_DLLEXPORT CASFunction *ASEXT_CreateCASFunction(aslScriptFunction *aslfn, CASModule *asmodule, int unk)
{
	return g_pfn_CASFunction_Create(aslfn, asmodule, unk);
}

C_DLLEXPORT CASServerManager *ASEXT_GetServerManager()
{
	return (*g_pServerManager);
}

C_DLLEXPORT bool ASEXT_CASRefCountedBaseClass_InternalRelease(void *ref)
{
	return g_pfn_CASRefCountedBaseClass_InternalRelease(ref);
}

C_DLLEXPORT void ASEXT_CScriptAny_Release(void *anywhat)
{
	g_pfn_CScriptAny_Release(anywhat);
}