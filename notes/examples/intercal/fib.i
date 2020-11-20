   DO .9 <- #16
    DO .10 <- #0
    DO .11 <- #1

(1) PLEASE READ OUT .11
    DO .1 <- .10
    DO .2 <- .11
    PLEASE (1009) NEXT
    DO .10 <- .11
    DO .11 <- .3

    DO (3) NEXT
    DO (1) NEXT

(3) DO (4) NEXT
    PLEASE GIVE UP

(4) DO .1 <- .9
    DO .2 <- #1
    PLEASE (1010) NEXT
    DO .9 <- .3
    DO .1 <- '.9~.9'~#1
    PLEASE (1020) NEXT
    DO RESUME .1
