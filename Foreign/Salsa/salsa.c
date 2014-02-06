/******************************************************************************
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Salsa low level requirements.
--
*****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef MONO

#include <stdint.h>
#include <mono/metadata/appdomain.h>

typedef struct {              //layout of structures can be found in mono's domain-internals.h
	MonoObject object;
	MonoString *application_base;
	MonoString *application_name;
	MonoString *cache_path;
	MonoString *configuration_file;
	MonoString *dynamic_base;
} MonoAppDomainSetupInternal;


typedef struct {
	void                        *lock;
  void                        *pad;
	void                        *mp;
	void                        *code_mp;
	MonoAppDomainSetupInternal  *setup;
	void                        *domain;
	void                        *default_context;

} MonoDomainInternal;


void setupDomain(MonoDomain *domain, char *baseDir, char *configFile) //workaround for an issue that was introduced since Mono 3.0 but yet unresolved
{
  MonoAppDomainSetupInternal *appDomSetup;
  appDomSetup = ((MonoDomainInternal*)domain)->setup; 
  if(appDomSetup != NULL)
  {
    appDomSetup->application_base = mono_string_new(domain, baseDir);
    appDomSetup->configuration_file = mono_string_new(domain, configFile);
  }
}

#else


#endif

#ifdef __cplusplus
}
#endif

