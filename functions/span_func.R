span_func = function(x) {
        
        if (x < 2) {span = 1} else 
                if (x < 5) {span = .8} else 
                        if (x < 7) {span = 0.65} else 
                                if (x < 9) {span = 0.5} else 
                                        if (x < 11) {span = 0.35} else
                                        if (x < 17) {span = 0.25}
        
        if (x < 3) {method = 'lm'} else 
                if (x < 17) {method = 'loess'}
        
        return(list("span" = span,
                    "method" = method))
}
