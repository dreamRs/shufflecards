CSS Playing Cards
=================

Playing cards using only CSS and Unicode characters

Usage
-----
All cards should be div elements, each with an empty span inside it. Each card gets a CSS class of "card", plus a class for the name of the suit (e.g. "card-hearts") and a class for the value (e.g. "card-9" for a nine). For instance, the king of spades would look like:
  &lt;div class="card card-spades card-k"&gt;&lt;span&gt;&lt;/span&gt;&lt;/div&gt;

A class of "card-joker" creates a joker; for games that need to differentiate between the two jokers, you can use "card-joke" and "card-joker-alt".

Additionally, adding class "card-facedown" to a card hides its face and shows the back of the card instead.

Customizing
-----------
All the dimensions and font sizes are built using ems as the unit, so you can easily adjust the size of the deck by adjusting the base font size of the .card class.

The colors and icons used for the suits, and the names of the face cards, are stored as LESS variables. These can be easily adjusted to customize the deck from its French-style default. For instance, changing one variable will turn hearts into the roses used instead in a Swiss German deck; another variable will turn those roses from red to Swiss German yellow.

Accessibility
-------------
For accessibility, each card can contain an optional &lt;p&gt; tag, into which you can put text descriptions of the card as appropriate (e.g. "9H" or "Nine of Hearts"). This text will not be visible in standard browsers, but will be read by screen readers.

Usage without LESS
------------------
A post-processed version of the .less file is included as a .css file. 
