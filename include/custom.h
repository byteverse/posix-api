#include <stddef.h>

// This include file lets us use hsc2hs to generate code working
// with ByteArray (through Data.Primitive) instead of Ptr.

// The macro FIELD_SIZEOF is defined in the linux kernel.
// It is written out here for portability.
#ifndef FIELD_SIZEOF
#define FIELD_SIZEOF(t, f) (sizeof(((t*)0)->f))
#endif


#define hsc_readByteArray(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> readByteArray hsc_arr (%ld + (hsc_ix * %ld)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_writeByteArray(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> writeByteArray hsc_arr (%ld + (hsc_ix * %ld)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_indexByteArray(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> indexByteArray hsc_arr (%ld + (hsc_ix * %ld)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_readByteArrayHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> readByteArray# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_writeByteArrayHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> writeByteArray# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_indexByteArrayHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> indexByteArray# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_readOffAddrHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> readOffAddr# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_writeOffAddrHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> writeOffAddr# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

#define hsc_indexOffAddrHash(t, f) \
  if ((offsetof(t,f)) % (FIELD_SIZEOF(t,f)) == 0 && (sizeof(t) % (FIELD_SIZEOF(t,f))) == 0) { \
    hsc_printf ("(\\hsc_arr hsc_ix -> indexOffAddr# hsc_arr (%ld# +# (hsc_ix *# %ld#)))", ((offsetof(t,f)) / (FIELD_SIZEOF(t,f))), (sizeof(t) / (FIELD_SIZEOF(t,f)))); \
  } else { \
    hsc_printf ("BAD_BYTEARRAY_ALIGNMENT"); \
  }

