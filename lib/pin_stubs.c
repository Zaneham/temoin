/* Affinity is Linux only. Elsewhere these report unsupported and the runner
   carries on unpinned, which is still a valid run, just a noisier one. */

/* _GNU_SOURCE has to come before any header at all, including OCaml's, or
   features.h is already in and the CPU_* macros never appear. */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <caml/mlvalues.h>

#if defined(__linux__)

#include <sched.h>

/* Pins the calling thread, so a domain pins itself and we never need to get
   at a pthread handle from outside. 1 on success, 0 on failure. */
CAMLprim value temoin_pin_self(value cpu)
{
  cpu_set_t set;
  int id = Int_val(cpu);

  if (id < 0 || id >= CPU_SETSIZE) return Val_int(0);

  CPU_ZERO(&set);
  CPU_SET(id, &set);
  return Val_int(sched_setaffinity(0, sizeof(set), &set) == 0 ? 1 : 0);
}

CAMLprim value temoin_pinning_supported(value unit)
{
  (void) unit;
  return Val_int(1);
}

#else

CAMLprim value temoin_pin_self(value cpu)
{
  (void) cpu;
  return Val_int(0);
}

CAMLprim value temoin_pinning_supported(value unit)
{
  (void) unit;
  return Val_int(0);
}

#endif
