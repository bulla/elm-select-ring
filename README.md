# elm-select-ring

An elm library for rings that may have selected elements.

Several modules are available.

## Select ring

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and selecting at most a single element from it.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.

## Multi select ring

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and optionally selecting multiple elements at once.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.
