#include <stddef.h>

// This include file lets us use hsc2hs to generate code working
// with ByteArray (through Data.Primitive) instead of Ptr.

// The macro FIELD_SIZEOF is defined in the linux kernel.
// It is written out here for portability.
#define INTERNAL_FIELD_SIZEOF(t, f) (sizeof(((t*)0)->f))

// This is a lot like peek except that it is designed to
// work with MutableByteArray instead of Ptr. GHC requires that all
// indexing into ByteArrays is aligned. So, we do some trickery
// based on the size of what the user is requesting.
#define hsc_read(t, f) \
  switch (INTERNAL_FIELD_SIZEOF(t,f)) { \
    case 1: \
      hsc_printf ("(\\hsc_arr -> readByteArray hsc_arr %ld)", (long) offsetof (t, f)); \
      break; \
    case 2: \
      if (offsetof (t, f) % 2 == 0) \
        hsc_printf ("(\\hsc_arr -> readByteArray hsc_arr %ld)", ((long) offsetof (t, f)) / 2); \
      else \
        hsc_printf ("BAD_READ_ALIGNMENT"); \
      break; \
    default: \
      hsc_printf ("(BAD_READ_SIZE_%ld)", (long) INTERNAL_FIELD_SIZEOF(t,f)); \
      break; \
  }

// This is like peek except that it is designed to work with ByteArray
// instead of Ptr. See hsc_read. This function becomes indexByteArray.
#define hsc_index(t, f) \
  switch (INTERNAL_FIELD_SIZEOF(t,f)) { \
    case 1: \
      hsc_printf ("(\\hsc_arr -> indexByteArray hsc_arr %ld)", (long) offsetof (t, f)); \
      break; \
    case 2: \
      if (offsetof (t, f) % 2 == 0) \
        hsc_printf ("(\\hsc_arr -> indexByteArray hsc_arr %ld)", ((long) offsetof (t, f)) / 2); \
      else \
        hsc_printf ("BAD_INDEX_ALIGNMENT"); \
      break; \
    case 4: \
      if (offsetof (t, f) % 4 == 0) \
        hsc_printf ("(\\hsc_arr -> indexByteArray hsc_arr %ld)", ((long) offsetof (t, f)) / 4); \
      else \
        hsc_printf ("BAD_INDEX_ALIGNMENT"); \
      break; \
    default: \
      hsc_printf ("(BAD_INDEX_SIZE_%ld)", (long) INTERNAL_FIELD_SIZEOF(t,f)); \
      break; \
  }

// This is a lot like poke except that it is designed to
// work with MutableByteArray instead of Ptr. See hsc_read.
#define hsc_write(t, f) \
  switch (INTERNAL_FIELD_SIZEOF(t,f)) { \
    case 1: \
      hsc_printf ("(\\hsc_arr -> writeByteArray hsc_arr %ld)", (long) offsetof (t, f)); \
      break; \
    case 2: \
      if (offsetof (t, f) % 2 == 0) \
        hsc_printf ("(\\hsc_arr -> writeByteArray hsc_arr %ld)", ((long) offsetof (t, f)) / 2); \
      else \
        hsc_printf ("BAD_WRITE_ALIGNMENT"); \
      break; \
    case 4: \
      if (offsetof (t, f) % 4 == 0) \
        hsc_printf ("(\\hsc_arr -> writeByteArray hsc_arr %ld)", ((long) offsetof (t, f)) / 4); \
      else \
        hsc_printf ("BAD_WRITE_ALIGNMENT"); \
      break; \
    default: \
      hsc_printf ("(BAD_WRITE_SIZE_%ld)", (long) INTERNAL_FIELD_SIZEOF(t,f)); \
      break; \
  }

// Compute an element offset from a byte offset and the element size.
// This causes a compile-time failure if the element size does not
// divide evenly into the byte offset. This is commonly used as:
#define hsc_elementize(boff, sz) \
  if ((boff) % (sz) == 0) { \
    hsc_printf ("%ld", ((boff) / (sz))); \
  } else { \
    hsc_printf ("BAD_ELEMENT_OFFSET"); \
  }

