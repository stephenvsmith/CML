# Wrapper function works (Sample with true DAG)

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    1    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    1    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] -1
    
    [[1]][[4]]
    [1] -1
    
    [[1]][[5]]
    [1] -1
    
    [[1]][[6]]
    [1] -1
    
    [[1]][[7]]
    [1] -1
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 0
    
    
    [[3]]
    [[3]][[1]]
    [1] -1
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 5
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 5
    
    
    [[4]]
    [[4]][[1]]
    [1] -1
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 5
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] -1
    
    [[4]][[6]]
    [1] -1
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] -1
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] -1
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] -1
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] -1
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] -1
    
    [[7]][[2]]
    [1] 0
    
    [[7]][[3]]
    [1] 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 164

---

     [1] 4 1 0 0 0 0 0 0 0 0 0

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    list()

---

               V1            V2            V3            V4            V5 
     1.128611e-16 -3.294413e-17  3.937302e-17 -3.662175e-17  7.779888e-17 
               V6            V7            V8 
    -8.336040e-17  1.214341e-16  9.956098e-17 

---

                V1          V2          V3          V4          V5          V6
    V1  1.00000000  0.18333386 -0.02845298  0.04514189  0.04652965 -0.11331008
    V2  0.18333386  1.00000000 -0.08214421  0.02150465  0.02487511 -0.22595043
    V3 -0.02845298 -0.08214421  1.00000000  0.30215847 -0.37867145 -0.18318288
    V4  0.04514189  0.02150465  0.30215847  1.00000000 -0.12098190 -0.75995045
    V5  0.04652965  0.02487511 -0.37867145 -0.12098190  1.00000000  0.09048158
    V6 -0.11331008 -0.22595043 -0.18318288 -0.75995045  0.09048158  1.00000000
    V7  0.06915716  0.16339239  0.12921911  0.48433298 -0.07562687 -0.61104468
    V8 -0.10685615 -0.12533246  0.17420443 -0.32199353 -0.70163762  0.48227208
                V7         V8
    V1  0.06915716 -0.1068561
    V2  0.16339239 -0.1253325
    V3  0.12921911  0.1742044
    V4  0.48433298 -0.3219935
    V5 -0.07562687 -0.7016376
    V6 -0.61104468  0.4822721
    V7  1.00000000 -0.2982090
    V8 -0.29820902  1.0000000

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] NA
    
    [[1]][[4]]
    [1] NA
    
    [[1]][[5]]
    [1] NA
    
    [[1]][[6]]
    [1] NA
    
    [[1]][[7]]
    [1] NA
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 0
    
    
    [[3]]
    [[3]][[1]]
    [1] NA
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 2
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 5
    
    
    [[4]]
    [[4]][[1]]
    [1] NA
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 2
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] -1
    
    [[4]][[6]]
    [1] -1
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] NA
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] -1
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] NA
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] -1
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] NA
    
    [[7]][[2]]
    [1] 0
    
    [[7]][[3]]
    [1] 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 115

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    list()

---

    [1] 2 1 0 0 0

---

               V1            V2            V3            V4            V5 
     1.128611e-16 -3.294413e-17  3.937302e-17 -3.662175e-17  7.779888e-17 
               V6            V7            V8 
    -8.336040e-17  1.214341e-16  9.956098e-17 

---

                V1          V2          V3          V4          V5          V6
    V1  1.00000000  0.18333386 -0.02845298  0.04514189  0.04652965 -0.11331008
    V2  0.18333386  1.00000000 -0.08214421  0.02150465  0.02487511 -0.22595043
    V3 -0.02845298 -0.08214421  1.00000000  0.30215847 -0.37867145 -0.18318288
    V4  0.04514189  0.02150465  0.30215847  1.00000000 -0.12098190 -0.75995045
    V5  0.04652965  0.02487511 -0.37867145 -0.12098190  1.00000000  0.09048158
    V6 -0.11331008 -0.22595043 -0.18318288 -0.75995045  0.09048158  1.00000000
    V7  0.06915716  0.16339239  0.12921911  0.48433298 -0.07562687 -0.61104468
    V8 -0.10685615 -0.12533246  0.17420443 -0.32199353 -0.70163762  0.48227208
                V7         V8
    V1  0.06915716 -0.1068561
    V2  0.16339239 -0.1253325
    V3  0.12921911  0.1742044
    V4  0.48433298 -0.3219935
    V5 -0.07562687 -0.7016376
    V6 -0.61104468  0.4822721
    V7  1.00000000 -0.2982090
    V8 -0.29820902  1.0000000

# Wrapper function works (Population)

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] -1
    
    [[1]][[4]]
    [1] -1
    
    [[1]][[5]]
    [1] 1
    
    [[1]][[6]]
    [1] 1
    
    [[1]][[7]]
    [1] 1
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 3 5
    
    
    [[3]]
    [[3]][[1]]
    [1] -1
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 2
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 4 5
    
    
    [[4]]
    [[4]][[1]]
    [1] -1
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 2
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] 3
    
    [[4]][[6]]
    [1] 3
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] 1
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] 3
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] 1
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] 3
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] 1
    
    [[7]][[2]]
    [1] 3 5
    
    [[7]][[3]]
    [1] 4 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 201

---

     [1] 2 2 0 0 0 0 0 0 0 0 0

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    list()

---

    [1] NA

---

    [1] NA

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] NA
    
    [[1]][[4]]
    [1] NA
    
    [[1]][[5]]
    [1] NA
    
    [[1]][[6]]
    [1] NA
    
    [[1]][[7]]
    [1] NA
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 3 5
    
    
    [[3]]
    [[3]][[1]]
    [1] NA
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 2
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 2 5
    
    
    [[4]]
    [[4]][[1]]
    [1] NA
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 2
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] 2
    
    [[4]][[6]]
    [1] 2
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] NA
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] 2
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] NA
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] 2
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] NA
    
    [[7]][[2]]
    [1] 3 5
    
    [[7]][[3]]
    [1] 2 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 131

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    list()

---

    [1] 2 1 0 0 0

---

    [1] NA

---

    [1] NA

# Wrapper function works (Sample)

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    0    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    1    0    0    0    0    1    1
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    0    1    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] -1
    
    [[1]][[4]]
    [1] -1
    
    [[1]][[5]]
    [1] -1
    
    [[1]][[6]]
    [1] -1
    
    [[1]][[7]]
    [1] -1
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 0
    
    
    [[3]]
    [[3]][[1]]
    [1] -1
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 5
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 5
    
    
    [[4]]
    [[4]][[1]]
    [1] -1
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 5
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] -1
    
    [[4]][[6]]
    [1] -1
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] -1
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] -1
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] -1
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] -1
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] -1
    
    [[7]][[2]]
    [1] 0
    
    [[7]][[3]]
    [1] 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 162

---

     [1] 4 1 0 0 0 0 0 0 0 0 0

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    1    0    0    1    0    0
    [5,]    0    0    1    0    0    1    0    1
    [6,]    0    1    0    1    1    0    1    1
    [7,]    0    0    0    0    0    1    0    0
    [8,]    0    0    0    0    1    1    0    0

---

               V1            V2            V3            V4            V5 
     1.128611e-16 -3.294413e-17  3.937302e-17 -3.662175e-17  7.779888e-17 
               V6            V7            V8 
    -8.336040e-17  1.214341e-16  9.956098e-17 

---

                V1          V2          V3          V4          V5          V6
    V1  1.00000000  0.18333386 -0.02845298  0.04514189  0.04652965 -0.11331008
    V2  0.18333386  1.00000000 -0.08214421  0.02150465  0.02487511 -0.22595043
    V3 -0.02845298 -0.08214421  1.00000000  0.30215847 -0.37867145 -0.18318288
    V4  0.04514189  0.02150465  0.30215847  1.00000000 -0.12098190 -0.75995045
    V5  0.04652965  0.02487511 -0.37867145 -0.12098190  1.00000000  0.09048158
    V6 -0.11331008 -0.22595043 -0.18318288 -0.75995045  0.09048158  1.00000000
    V7  0.06915716  0.16339239  0.12921911  0.48433298 -0.07562687 -0.61104468
    V8 -0.10685615 -0.12533246  0.17420443 -0.32199353 -0.70163762  0.48227208
                V7         V8
    V1  0.06915716 -0.1068561
    V2  0.16339239 -0.1253325
    V3  0.12921911  0.1742044
    V4  0.48433298 -0.3219935
    V5 -0.07562687 -0.7016376
    V6 -0.61104468  0.4822721
    V7  1.00000000 -0.2982090
    V8 -0.29820902  1.0000000

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    0    0    0    0    0
    [4,]    0    0    0    0    0    1    0    0
    [5,]    0    0    0    0    0    0    0    1
    [6,]    0    1    0    1    0    0    1    1
    [7,]    0    0    0    0    0    1    0    0
    [8,]    0    0    0    0    0    0    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] NA
    
    [[1]][[3]]
    [1] NA
    
    [[1]][[4]]
    [1] NA
    
    [[1]][[5]]
    [1] NA
    
    [[1]][[6]]
    [1] NA
    
    [[1]][[7]]
    [1] NA
    
    
    [[2]]
    [[2]][[1]]
    [1] NA
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 0
    
    
    [[3]]
    [[3]][[1]]
    [1] NA
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 2
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 5
    
    
    [[4]]
    [[4]][[1]]
    [1] NA
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 2
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] -1
    
    [[4]][[6]]
    [1] -1
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] NA
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] -1
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] NA
    
    [[5]][[7]]
    [1] NA
    
    
    [[6]]
    [[6]][[1]]
    [1] NA
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] -1
    
    [[6]][[5]]
    [1] NA
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] NA
    
    [[7]][[2]]
    [1] 0
    
    [[7]][[3]]
    [1] 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] NA
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

    [1] 112

---

    [1] 1 2 4 5 6 7 8

---

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    1    0    0    0    0    0    0
    [2,]    1    0    0    0    0    1    0    0
    [3,]    0    0    0    1    1    0    0    0
    [4,]    0    0    1    0    0    1    0    0
    [5,]    0    0    1    0    0    1    0    1
    [6,]    0    1    0    1    1    0    1    1
    [7,]    0    0    0    0    0    1    0    0
    [8,]    0    0    0    0    1    1    0    0

---

    [1] 1 0 0 0 0

---

               V1            V2            V3            V4            V5 
     1.128611e-16 -3.294413e-17  3.937302e-17 -3.662175e-17  7.779888e-17 
               V6            V7            V8 
    -8.336040e-17  1.214341e-16  9.956098e-17 

---

                V1          V2          V3          V4          V5          V6
    V1  1.00000000  0.18333386 -0.02845298  0.04514189  0.04652965 -0.11331008
    V2  0.18333386  1.00000000 -0.08214421  0.02150465  0.02487511 -0.22595043
    V3 -0.02845298 -0.08214421  1.00000000  0.30215847 -0.37867145 -0.18318288
    V4  0.04514189  0.02150465  0.30215847  1.00000000 -0.12098190 -0.75995045
    V5  0.04652965  0.02487511 -0.37867145 -0.12098190  1.00000000  0.09048158
    V6 -0.11331008 -0.22595043 -0.18318288 -0.75995045  0.09048158  1.00000000
    V7  0.06915716  0.16339239  0.12921911  0.48433298 -0.07562687 -0.61104468
    V8 -0.10685615 -0.12533246  0.17420443 -0.32199353 -0.70163762  0.48227208
                V7         V8
    V1  0.06915716 -0.1068561
    V2  0.16339239 -0.1253325
    V3  0.12921911  0.1742044
    V4  0.48433298 -0.3219935
    V5 -0.07562687 -0.7016376
    V6 -0.61104468  0.4822721
    V7  1.00000000 -0.2982090
    V8 -0.29820902  1.0000000

---

    [1] 3

---

    [1] 3

# Discrete version

         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
    [1,]    0    0    0    0    0    0    0    0
    [2,]    0    0    0    1    1    0    0    0
    [3,]    0    0    0    0    0    1    0    0
    [4,]    0    1    0    0    0    1    0    0
    [5,]    0    1    0    0    0    0    0    1
    [6,]    0    0    0    0    0    0    0    0
    [7,]    0    0    0    0    0    0    0    0
    [8,]    0    0    0    0    1    0    0    0

---

    [[1]]
    [[1]][[1]]
    [1] NA
    
    [[1]][[2]]
    [1] -1
    
    [[1]][[3]]
    [1] NA
    
    [[1]][[4]]
    [1] NA
    
    [[1]][[5]]
    [1] 3
    
    [[1]][[6]]
    [1] 3
    
    [[1]][[7]]
    [1] 3 4
    
    
    [[2]]
    [[2]][[1]]
    [1] -1
    
    [[2]][[2]]
    [1] NA
    
    [[2]][[3]]
    [1] -1
    
    [[2]][[4]]
    [1] -1
    
    [[2]][[5]]
    [1] NA
    
    [[2]][[6]]
    [1] 5
    
    [[2]][[7]]
    [1] 5
    
    
    [[3]]
    [[3]][[1]]
    [1] NA
    
    [[3]][[2]]
    [1] -1
    
    [[3]][[3]]
    [1] NA
    
    [[3]][[4]]
    [1] 1
    
    [[3]][[5]]
    [1] NA
    
    [[3]][[6]]
    [1] 5
    
    [[3]][[7]]
    [1] 5
    
    
    [[4]]
    [[4]][[1]]
    [1] NA
    
    [[4]][[2]]
    [1] -1
    
    [[4]][[3]]
    [1] 1
    
    [[4]][[4]]
    [1] NA
    
    [[4]][[5]]
    [1] 1
    
    [[4]][[6]]
    [1] 1
    
    [[4]][[7]]
    [1] NA
    
    
    [[5]]
    [[5]][[1]]
    [1] 3
    
    [[5]][[2]]
    [1] NA
    
    [[5]][[3]]
    [1] NA
    
    [[5]][[4]]
    [1] 1
    
    [[5]][[5]]
    [1] NA
    
    [[5]][[6]]
    [1] 2 3
    
    [[5]][[7]]
    [1] 2 3
    
    
    [[6]]
    [[6]][[1]]
    [1] 3
    
    [[6]][[2]]
    [1] 5
    
    [[6]][[3]]
    [1] 5
    
    [[6]][[4]]
    [1] 1
    
    [[6]][[5]]
    [1] 2 3
    
    [[6]][[6]]
    [1] NA
    
    [[6]][[7]]
    [1] 5
    
    
    [[7]]
    [[7]][[1]]
    [1] 3 4
    
    [[7]][[2]]
    [1] 5
    
    [[7]][[3]]
    [1] 5
    
    [[7]][[4]]
    [1] NA
    
    [[7]][[5]]
    [1] 2 3
    
    [[7]][[6]]
    [1] 5
    
    [[7]][[7]]
    [1] NA
    
    

---

     [1] 1 0 0 0 0 0 0 0 0 0 0

