Glyph generator instructions
================

# Intro

This app is a graphical user interface for the `glyph` R package.
Currently, the package implements three algorithms to produce generative
art in the style of glyphs inspired by the game Gris. The three
algorithms are listed in the first selection menu **“Type of glyph to
generate”**.

  - *Orbital glyph* will make a glyph that has a “seed” surrounded by
    concentric circles and some attached planets, which appear as dots
    on top of certain circles.
  - *Summoning glyph* will make a glyph that looks more like a summoning
    circle or astronomical chart. This glyph has a “seed” surrounded by
    only a couple of circles, but it has shapes inscribed within those
    circles
  - *Glitched glyph* will make a glyph that is a base of concentric
    circles, but has some type of “glitch” which introduces randomness
    in the circles.

# Detailed parameters

## Orbital glyph

  - *Seed probabilities* These are the probabilities of choosing a
    different shape for the “seed” which lies in the center of each
    glyph. The options are “none”, “circle”, “diamond”, and “square”.
    The number you choose for each one corresponds to the probability
    that that shape will be sampled. It makes the most sense to keep
    these 4 numbers adding to 1, but it is possible to adjust them to
    different ranges.
  - *Probability of second set of orbits* This algorithm will always
    generate at least one “set” of rings around the seed. It has a
    probability to generate a second set of rings beginning slightly
    further out from the original, you can adjust the probability of
    generating this second set here.

## Summoning glyph

  - *Seed probabilities* These are the probabilities of choosing a
    different shape for the “seed” which lies in the center of each
    glyph. The options are “none”, “circle”, “diamond”, and “square”.
    The number you choose for each one corresponds to the probability
    that that shape will be sampled. It makes the most sense to keep
    these 4 numbers adding to 1, but it is possible to adjust them to
    different ranges.
  - *Probabilities of number of inscribed shapes* Each summoning glyph
    has a probability to draw further shapes (circles, diamonds,
    squares) inside of the outer rings. You can adjust the probability
    of how many of these shapes will be drawn here.
  - *Probabilities of more sets of inscribed shapes* There is also a
    possibility to draw a second or third “set” of inscribed shapes,
    which will usually be rotated so as not to overlap with the original
    set. Here you can set the probability of whether or not to draw a
    second or third set of inscribed shapes.

## Glitched glyph

  - *Glitch type* There are three types of glitches. *Spike* will cause
    random spikes around the orbits of this glyph. *Connected* will
    cause sections of orbits to jut out or shrink in from their original
    position, but remain connected to as one line. *Shattered* will also
    cause jutting out of sections of orbits, but it will also
    disconnected lines where they jut out to make the glyph appear more
    disconnected or shattered
  - *Seed probabilities* These are the probabilities of choosing a
    different shape for the “seed” which lies in the center of each
    glyph. The options are “none”, “circle”, “diamond”, and “square”.
    The number you choose for each one corresponds to the probability
    that that shape will be sampled. It makes the most sense to keep
    these 4 numbers adding to 1, but it is possible to adjust them to
    different ranges.
  - *Probability of second set of orbits* This algorithm will always
    generate at least one “set” of rings around the seed. It has a
    probability to generate a second set of rings beginning slightly
    further out from the original, you can adjust the probability of
    generating this second set here.
  - *Glitch parameters* These control various aspects of how the glyph
    should be glitched. Some apply to all glitch options, while others
    only apply to specific options. *Number of glitches* applys to the
    connected and shattered glyphs, and controls how many regions are
    jutting out. *Min/max glitch radius* applies to all glitch types and
    controls the maximum and minimum distance to grow or shrink part of
    the orbit. *Min/max number of spikes* applies to spike glyphs and
    controls the number. *Min/max jitter of spikes* applies to spike
    glyphs and controls how “normal” the spike should look. This means
    that if you set a very low jitter, the spikes will just look like
    spikes; if you set a higher jitter, the spikes will distort and be
    jutting off to a side or may be mishapen like random polygons.
