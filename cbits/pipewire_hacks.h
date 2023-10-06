#include <pipewire/pipewire.h>

// Pipewire defines some functions via macros, which can't be directly invoked via c2hs,
// so we need these wrapper functions.
//
// Might be fixed in future: https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/2808

struct pw_registry* pw_core_get_registry_(
  struct pw_core *core,
  uint32_t version,
  size_t user_data_size);

void pw_registry_add_listener_(
  struct pw_registry* registry,
  struct spa_hook* listener,
  struct pw_registry_events* events);
