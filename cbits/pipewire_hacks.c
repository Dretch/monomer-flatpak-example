#include <pipewire_hacks.h>

struct pw_registry* pw_core_get_registry_(
    struct pw_core* core,
    uint32_t version,
    size_t user_data_size) {

  return pw_core_get_registry(core, version, user_data_size);
}

void pw_registry_add_listener_(
  struct pw_registry* registry,
  struct spa_hook* listener,
  struct pw_registry_events* events) {
  
  pw_registry_add_listener(registry, listener, events, NULL);
}