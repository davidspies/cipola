# cipola

Example Usage:

```
$ stack ghci --no-load --ghci-options "-O -fobject-code"
> :load Lib
> modRoot 132 123456
([16770,52674,9054,44958,39918,14094,47634,21810],61728)
```

This is saying that `x² ≡ 132 (mod 123456)` if and only if `x ≡ k (mod 61728)` where `k ∈ {16770,52674,9054,44958,39918,14094,47634,21810}`

---

Note the type of `modRoot`

```
> import PrimeVector
> :t modRoot
modRoot :: Integer -> PrimeVector -> ([Integer], PrimeVector)
```

The second argument is a `PrimeVector` (represented internally of a list of prime-exponent pairs) which has a `Num` instance and `Show` instance. This is how you can give the argument as a single decimal number. However, the implicit `fromInteger` call made when you pass the literal expression `123456` does Lenstra Elliptic Curve factorization under the hood. If you already know the factorization of your modulus, you can skip this step by passing in the prime decomposition yourself:

```
> m = 2847918832900147128347298183112903171823131309 * 3422831979814812123388391922932943548176658773 :: PrimeVector
> modRoot 3 m
([2990777725003092441095250021829821255226672582768790891440417561790420424675664376309423768,789222666850244127301065216667322350810117235226078699566816488968324789919982128876461468,8958724990317255568884244223268168385279475786374291386112137515787541654770444753189362389,6757169932164407255090059418105669480862920438831579194238536442965446020014762505756400089],9747947657167499696185309439935490736089593021600370085678954004755866444690426882065823857)
```

And instead of having to factorize the number, it only needs to run a Miller-Rabin test on each of the factors to ensure that they are indeed prime.

---

In some cases a number has a lot of roots (eg. when `a` and `n` are relatively prime and `n` is odd, then if `a` has any square roots it will have `2^k` square roots where `k=`# distinct prime factors in `n`). Thankfully, the list of possible roots is constructed lazily so you can get a partial list if you don't need all of them and you don't want to waste time constructing the others:

```
> :{
| let factorial :: Integer -> PrimeVector
|     factorial 0 = 1
|     factorial n = fromInteger n * factorial (n - 1)
| :}
> let (xs, n) = modRoot 1 (factorial 100)
> take 5 xs
[1,62538185606766700250623201295436448782438535434894900984108687146279835047009737263723661427321378233056601699523397363776567409377280000000000000000000000001,56624894763741396009120886497060694679760250407602332127011461239794948310499049920031899491151145926491007727243484078040044959432704000000000000000000000001,25836864926563943578044848936230442971482817578115611642527184490857183364278871574814096942316005873293911505939657683565427157893120000000000000000000000001,34856779262196008832923812102942984520628855616817232114775685310261995178194305829845607027239181528600778741513782367539599295643648000000000000000000000001]
```
