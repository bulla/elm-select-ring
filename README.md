# elm-select-ring

An elm library for rings that may have selected elements.

Several modules are available.

## Focus ring

This module provides a form of array that allows navigating through its elements as if it was
a ring and focusing on a single element.

Moving the cursor around allows to focus on a single element.
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.

## Zipper ring

This **experimental** module provides a form of non-empty focused ring that uses a zipper list as 
its internal structure instead of an array.

Moving the focus around allows to focus on a single element.
When the focus is at the end, focusing on the next element will return focus to the first element.
When the focus is at the beginning, focusing on the previous element will return focus to the last
element.


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

# Examples

1. In-game character selector example using a FocusRing. See examples/CharacterSelect.elm file.
2. Color picker example using a FocusRing with elm-css. See examples/ColorPicker.elm file.
3. In-game item selector example using a SelectRing. See examples/SelectRing.elm file.
4. Yahtzee-like dice game example using a MultiSelectRing. See examples/DiceSelect.elm file.

## Resources

- Useful tool for animated GIFs edition: https://ezgif.com/
- Identify Unicode characters: http://shapecatcher.com/
- Credits to Neslug for the Mario GIFs: https://www.deviantart.com/neslug/gallery/3553116/super-mario
- Dice images: https://www.shutterstock.com/image-illustration/3d-rendering-set-six-black-dice-751136791
