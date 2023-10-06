{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.PipeWire where

#include <pipewire/pipewire.h>
#include <pipewire_hacks.h>

import Bindings.Util
import Data.Text (Text)
import Foreign (FunPtr, Ptr, Storable (..))
import Foreign.C (CInt, CString, CUInt)
import Foreign.Marshal.Utils (with)

data DictItem = DictItem {key :: CString, value :: CString}

{#pointer *spa_dict_item as DictItemPtr -> DictItem#}

instance Storable DictItem where
  sizeOf _ = {#sizeof spa_dict_item#}
  alignment _ = {#alignof spa_dict_item#}
  peek p = do
    DictItem <$> {#get spa_dict_item->key#} p
             <*> {#get spa_dict_item->value#} p
  poke p a = do
    {#set spa_dict_item->key#} p a.key
    {#set spa_dict_item->value#} p a.value

data Dict = Dict {flags :: CUInt, nItems :: CUInt, items :: Ptr DictItem}

{#pointer *spa_dict as DictPtr -> Dict#}

instance Storable Dict where
  sizeOf _ = {#sizeof spa_dict#}
  alignment _ = {#alignof spa_dict#}
  peek p = do
    Dict <$> {#get spa_dict->flags#} p
         <*> {#get spa_dict->n_items#} p
         <*> {#get spa_dict->items#} p
  poke p a = do
    {#set spa_dict->flags#} p a.flags
    {#set spa_dict->n_items#} p a.nItems
    {#set spa_dict->items#} p a.items

{#pointer *spa_hook as SpaHook#}

spaHookSize :: Int
spaHookSize = {#sizeof spa_hook#}

data RegistryEvents = RegistryEvents
  { version :: CUInt
  , globalCallback :: FunPtr (Ptr () -> CUInt -> CUInt -> CString -> CUInt -> DictPtr -> IO ())
  , globalRemoveCallback :: FunPtr (Ptr () -> CUInt -> IO ())
  }

instance Storable RegistryEvents where
  sizeOf _ = {#sizeof pw_registry_events#}
  alignment _ = {#alignof pw_registry_events#}
  peek p = do
    version <- {#get pw_registry_events->version#} p
    globalCallback <- {#get pw_registry_events->global#} p
    globalRemoveCallback <- {#get pw_registry_events->global_remove#} p
    pure RegistryEvents {version, globalCallback, globalRemoveCallback}
  poke p a = do
    {#set pw_registry_events->version#} p a.version
    {#set pw_registry_events->global#} p a.globalCallback
    {#set pw_registry_events->global_remove#} p a.globalRemoveCallback

{#pointer *pw_registry_events as RegistryEventsPtr -> RegistryEvents#}

versionRegistry :: CUInt
versionRegistry = {#const PW_VERSION_REGISTRY#}

versionRegistryEvents :: CUInt
versionRegistryEvents = {#const PW_VERSION_REGISTRY_EVENTS#}

foreign import ccall "wrapper"
  registryEventsGlobalCallback ::
    (Ptr () -> CUInt -> CUInt -> CString -> CUInt -> DictPtr -> IO ()) ->
    IO (FunPtr (Ptr () -> CUInt -> CUInt -> CString -> CUInt -> DictPtr -> IO ()))

foreign import ccall "wrapper"
  registryEventsGlobalRemoveCallback ::
    (Ptr () -> CUInt -> IO ()) ->
    IO (FunPtr (Ptr () -> CUInt -> IO ()))

{#pointer *pw_main_loop as MainLoop newtype#}

{#pointer *pw_loop as Loop newtype#}

{#pointer *pw_properties as Properties newtype#}

{#pointer *pw_proxy as Proxy newtype#}

{#pointer *pw_stream as Stream newtype#}

{#pointer *pw_core as Core newtype#}

{#pointer *pw_context as Context newtype#}

{#pointer *pw_registry as Registry newtype#}

{#fun unsafe pw_init as init {id `Ptr CInt', id `Ptr (Ptr CString)'} -> `()' #}

{#fun unsafe pw_deinit as deinit {} -> `()'#}

{#fun unsafe pw_main_loop_new as mainLoopNew {`DictPtr'} -> `MainLoop'#}

{#fun unsafe pw_main_loop_get_loop as mainLoopGetLoop {`MainLoop'} -> `Loop'#}

{#fun pw_main_loop_run as mainLoopRun {`MainLoop'} -> `CInt'#}

{#fun unsafe pw_main_loop_destroy as mainLoopDestroy {`MainLoop'} -> `()'#}

{#fun unsafe pw_main_loop_quit as mainLoopQuit {`MainLoop'} -> `CInt'#}

{#fun unsafe pw_stream_destroy as streamDestroy {`Stream'} -> `()'#}

{#fun unsafe pw_core_get_registry_ as coreGetRegistry {`Core', `CUInt', `CUInt'} -> `Registry'#}

{#fun unsafe pw_core_disconnect as coreDisconnect {`Core'} -> `CInt'#}

{#fun unsafe pw_registry_add_listener_ as registryAddListener {`Registry', `SpaHook', with* `RegistryEvents'} -> `()'#}

{#fun unsafe pw_context_new as contextNew {`Loop', `Properties', `CUInt'} -> `Context'#}

{#fun unsafe pw_context_connect_fd as contextConnectFd {`Context', `CInt', `Properties', `CInt'} -> `Core'#}

{#fun unsafe pw_context_destroy as contextDestroy {`Context'} -> `()'#}

{#fun unsafe variadic pw_properties_new[const char *, const char *] as propertiesNew1 {withText* `Text', withText* `Text', withNull- `CString'} -> `Properties'#}

{#fun unsafe variadic pw_properties_new[const char *, const char *, const char *, const char *] as propertiesNew2 {withText* `Text', withText* `Text', withText* `Text', withText* `Text', withNull- `CString'} -> `Properties'#}

{#fun unsafe variadic pw_properties_new[const char *, const char *, const char *, const char *, const char *, const char *] as propertiesNew3 {withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withNull- `CString'} -> `Properties'#}

{#fun unsafe variadic pw_properties_new[const char *, const char *, const char *, const char *, const char *, const char *, const char *, const char *] as propertiesNew4 {withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withText* `Text', withNull- `CString'} -> `Properties'#}

{#fun unsafe pw_properties_set as propertiesSet {`Properties', withText* `Text', withText* `Text'} -> `CInt'#}

{#fun unsafe pw_proxy_destroy as proxyDestroy {`Proxy'} -> `()'#}
