 1105  g(x)=a2*x**b2+c2
 1106  f(x)=a1*x**b1+c1
 1107  fit g(x) '<tail -2000 hash.lst' using 2:1 via a2,b2,c2
 1108  fit f(x) '<tail -1000 list.lst' using 2:1 via a1,b1,c1
 1109  plot 'hash.lst' using 2:1, g(x), 'list.lst' using 2:1, f(x)
 1110  history
