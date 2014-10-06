> import Graphics.Rendering.Chart.Easy
> import Text.Lit.ChartDiagrams
>
> signal :: [Double] -> [(Double,Double)]
> signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
>
> thePlot = do
>     layout_title .= "Amplitude Modulation"
>     plot (line "am" [signal [0,(0.5)..400]])
>     plot (points "am points" (signal [0,7..400]))

```splice
    ecChart "Test plot" thePlot
```
