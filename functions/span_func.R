span_func = function(x) {
        
        if (x < 5) {span = 1} else 
                if (x < 7) {span = 1} else 
                        # if (x < 8) {span = 0.75} else 
                        #         if (x < 9) {span = 0.5} else 
                        #                 if (x < 11) {span = 0.35} else
                                        if (x < 17) {span = 0.25}
        
        if (x < 5) {method = 'lm'} else 
                if (x < 17) {method = 'loess'}
        
        return(list("span" = span,
                    "method" = method))
}
