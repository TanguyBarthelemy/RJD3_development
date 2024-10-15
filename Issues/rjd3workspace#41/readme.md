## Issue rjdverse/rjd3workspace#41

Ici c'est peut-être un pb de set_specification.
Il faut réussir à remontrer la trace de l'appel :

``` r
.jcall(jsap, "V", "setSpecification", as.integer(0),
       jspec)
```

Le problème touche aussi bien les spec x13 que tramoseats.

Erreur :

```
Error in .jcall(jsap, "V", "setSpecification", as.integer(1), jspec) : 
  method setSpecification with signature (ILjdplus/sa/base/api/SaSpecification;)V not found
```
