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



void setupDomain(MonoDomain *domain, char *baseDir, char *configFile) //workaround for an issue that was introduced since Mono 3.0 but yet unresolved: https://bugzilla.xamarin.com/show_bug.cgi?id=12669
{
  // Mono issue introduced in 3.0 (https://bugzilla.xamarin.com/show_bug.cgi?id=12669) has been fixed (https://github.com/mono/mono/commit/57f5187ad29a7913f083a659ea77d90eb8bad4d4)
  // Using this instead of rolling our own means we depend on >= Mono-3.8
  mono_domain_set_config(domain, baseDir, configFile);
}

#else


#endif

#ifdef __cplusplus
}
#endif

