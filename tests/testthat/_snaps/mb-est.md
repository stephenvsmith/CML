# Neighborhood Estimation

    [1] 4 5

---

    [1] 3 4 6 8

---

    [1] 2 4 5 7 8

---

    [1] 5 6

---

    [1] 3 5 6

---

    [1] 1 6

---

    [1] 6

---

    Creating the reference DAG using Markov Blanket list.
    Nodes being considered: 1,2,3,4,5,6,7,8 
    

# Verbose arguments

    Creating the reference DAG using Markov Blanket list.
    Nodes being considered: 1,2,3,4,5,6,7,8 
    

# Test spouse recovery function (one target)

    Checking if node 1 is a spouse of target 6 ... no
    Checking if node 3 is a spouse of target 6 ... no
    Checking if node 5 is a spouse of target 6 ... yes. Adding 5 to MB of 6 . 5 is a newly discovered 1st-order neighbor (was previously 2nd-order).
    $target_mbs
    $target_mbs$`6`
    $target_mbs$`6`$mb
    [1] 2 4 5 7 8
    
    
    
    $f_o_mbs
    $f_o_mbs$`2`
    $f_o_mbs$`2`$mb
    [1] 1 6
    
    
    $f_o_mbs$`4`
    $f_o_mbs$`4`$mb
    [1] 6 3
    
    
    $f_o_mbs$`7`
    $f_o_mbs$`7`$mb
    [1] 6
    
    
    $f_o_mbs$`8`
    $f_o_mbs$`8`$mb
    [1] 6 5
    
    
    
    $spouse_mbs
    $spouse_mbs$`5`
    [1] 6
    
    
    $num_tests
    [1] 3
    
    $spouses_added
    [1] 5
    

# Test spouse recovery function (two targets) 1

    Checking if node 1 is a spouse of target 6 ... no
    Checking if node 3 is a spouse of target 6 ... no
    Checking if node 5 is a spouse of target 6 ... yes. Adding 5 to MB of 6 . Adding 6 to MB of 5 (first-order neighbor).
    Checking if node 2 is a spouse of target 8 ... no
    Checking if node 3 is a spouse of target 8 ... no
    Checking if node 4 is a spouse of target 8 ... no
    Checking if node 7 is a spouse of target 8 ... no
    $target_mbs
    $target_mbs$`6`
    $target_mbs$`6`$mb
    [1] 2 4 5 7 8
    
    
    $target_mbs$`8`
    $target_mbs$`8`$mb
    [1] 6 5
    
    
    
    $f_o_mbs
    $f_o_mbs$`2`
    $f_o_mbs$`2`$mb
    [1] 1 6
    
    
    $f_o_mbs$`4`
    $f_o_mbs$`4`$mb
    [1] 6 3
    
    
    $f_o_mbs$`7`
    $f_o_mbs$`7`$mb
    [1] 6
    
    
    $f_o_mbs$`5`
    $f_o_mbs$`5`$mb
    [1] 3 6 8
    
    
    
    $spouse_mbs
    list()
    
    $num_tests
    [1] 7
    
    $spouses_added
    NULL
    

# Test spouse recovery function (multiple targets)

    Checking if node 4 is a spouse of target 2 ... yes. Adding 4 to MB of 2 . Adding 2 to MB of 4 (another target node).
    Checking if node 7 is a spouse of target 2 ... no
    Checking if node 8 is a spouse of target 2 ... no
    Checking if node 1 is a spouse of target 4 ... no
    Checking if node 5 is a spouse of target 4 ... no
    Checking if node 7 is a spouse of target 4 ... no
    Checking if node 8 is a spouse of target 4 ... no
    Checking if node 1 is a spouse of target 6 ... no
    Checking if node 3 is a spouse of target 6 ... no
    Checking if node 5 is a spouse of target 6 ... yes. Adding 5 to MB of 6 . Adding 6 to MB of 5 (another target node).
    Checking if node 2 is a spouse of target 5 ... no
    Checking if node 7 is a spouse of target 5 ... no

# Markov Blanket Estimation

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 0.8

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 0.6666667

---

    [1] 1

---

    [1] 0.8

---

    [1] 1

---

    [1] 1

---

    [1] 1

---

    [1] 1

# All Markov Blankets (multiple targets)

    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 4 : 3,6 
    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 8 : 5,6 
    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 3 : 4,5 
    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 6 : 2,4,7,8 
    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 5 : 3,8 
    Checking if node 2 is a spouse of target 4 ... yes. Adding 2 to MB of 4 . 2 is a newly discovered 1st-order neighbor (was previously 2nd-order).
    Checking if node 5 is a spouse of target 4 ... no
    Checking if node 7 is a spouse of target 4 ... no
    Checking if node 8 is a spouse of target 4 ... no
    Checking if node 2 is a spouse of target 8 ... no
    Checking if node 3 is a spouse of target 8 ... no
    Checking if node 7 is a spouse of target 8 ... no
    Estimating Markov Blankets using
     Algorithm: MMPC 
     Test: testIndFisher 
     Tolerance: 0.01 
    Results for target 2 : 1,6 
    Adding target nodes to spouse's MB List: 4 

