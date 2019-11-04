# Haze - Bokeh visualization for interactive stack project

## What's Haze

Haze implements a
[DSL](https://en.wikipedia.org/wiki/Domain-specific_language)
within Haskell, having
[BokehJS](https://docs.bokeh.org/en/latest/docs/dev_guide/bokehjs.html)
(see [Bokeh](https://bokeh.org))
as the transpiling target, for Haskell code to do low-level
plotting (in contrast to more declarative, high-level charting).

## Advantage

Size & Speed in essence.

- Actuall data are always transfered & used as binary chunks, much more
  efficient than solutions with intermediate representations in textual
  (commonly JSON in particular) form. This unleashes Bokeh's potental
  of rendering massive data points at the order of millions, while still
  being smooth, as well as previewed, during continuous scaling (zoom),
  and dragging (pan), and etc. BokehJS achieves these by rendering with
  [WebGL](https://www.khronos.org/webgl/) beyond HTML Canvas, or simply
  SVG.

- You can interactively plot the massive result data of your Haskell
  code (tends under active development in a stack project) for immediate
  verification & intuition.

- The plotting page can be right-away published to a trusted networked
  environment (with
  [security CAVEAT](https://github.com/complyue/hadui/issues/3)
  ).

## Style

[BokehJS](https://docs.bokeh.org/en/latest/docs/user_guide/bokehjs.html)
aims to be declarative compared to traditional graphical programming
toolkits, but it still feels imperative compared to Haskell stylish.

Haze might has more space to improve, let's see how's that over time.

## Documentation

Haze lacks documentation as badly as BokehJS does, best doc may be the
(unfortunately Python oriented)
[API reference](https://docs.bokeh.org/en/latest/docs/reference.html).

While fortunately it'll feel language-neutral once you get familiar
with Bokeh at all, the reference sorta works for all languages relevant,
i.e. TypeScript (JavaScript), Python, and more languages listed at
https://docs.bokeh.org/en/latest/docs/dev_guide/bindings.html

## Design

Haze works as a
[Hadui](https://github.com/complyue/hadui)
[overlay](https://github.com/complyue/hadui/wiki/OverlayPackage).

Haze deviates from the strongly typed tradition pursued by both Bokeh
(written in _TypeScript_ at the core) and Haskell, for time being.
It may possibly become a full-fledged, strongly typed
[Haskell](https://www.haskell.org/) binding of [Bokeh](https://bokeh.org)
ultimately, but however not for certain right now.
