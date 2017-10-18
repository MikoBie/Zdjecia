x <- c(1, 15, 100, 120, 7)

### Pętla po ciągu indeksów wektora
for (i in 1:length(x)) {
    print(i)
}

### Pętla po elementach wektora
for (i in x) {
    print(i)
}


### Indeksowanie
letters[5:10]
letters[c(2, 5, 8, 9)]
letters[length(letters):1]
letters[-1]
letters[-c(2, 5, 7)]
letters[-(1:10)]
letters[1:10][3:4][-2]

### Powtarzanie wektorów
rep(letters[1:3], 10)
rep(letters[1:3], each = 10)

### Wektory z nazwami
named <- c(
    A = 1,
    B = 2,
    C = 3
)
