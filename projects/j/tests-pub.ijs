NB. TESTING SETUP
NB. (also consider https://github.com/jsoftware/general_unittest)

NB. "display" utility func
display =: (1!:2) & 2
NB. ensure assertions are enabled (not needed?)
enable_assert =: 3 : 0
    if. 0 = 9!:34 '' do. (9!:35) 1 end.
)
enable_assert''
NB. basic assert (overwrites already-given assert verb)
NB.   failure -> 1, success -> 0
assert =: 3 : 0
    if. y do. 0
    else.
        display '***** Encountered an error! *****'
        NB. display something?
        1
    end.
)
NB. make sure it works
NB. assert_test =: 3 : 0
NB.     assert. (assert (0=1))=1
NB.     assert. (assert (1=1))=0
NB. )
NB. assert_test''

NB. load the student file (into a new locale, so they don't overwrite our stuff)
(18 !: 4) < 'student'
(0  !: 0) < 'hw.ijs'
(18 !: 4) < 'base'

NB. get boolean funcs
and =: *. NB. (1 b.) also works
or  =: +.
NB. now we can and/ all of our assertions for the tests

NB. PUBLIC TESTS
NB. note that testing order should not matter
NB.   student functions are in the 'student' locale,
NB.   should access func f like: f_student_

NB. test_00 =: 3 : 0
NB.     assert 1=1
NB. )

test_hello =: 3 : 0
    assert hello_student_ = 'Hello, World!'
)

test_mult =: 3 : 0
    and /
        assert (123 mult_student_ 456) = 56088
        assert (1 2 3 mult_student_ 4 5 6) = 4 10 18
)

test_pow =: 3 : 0
    and /
        assert (2 pow_student_ 3) = 8
        assert (_1 pow_student_ 2) = 1
        assert (1 2 3 pow_student_ 1 2 3) = 1 4 27
)

test_BackwardsBoolean =: 3 : 0
    and /
        assert (BackwardsBoolean_student_ 1) = 1
        assert (BackwardsBoolean_student_ 0) = 0
        assert (BackwardsBoolean_student_ 'true') = 0
        assert (BackwardsBoolean_student_ 'false') = 0
)

test_tf =: 3 : 0
    and /
        assert (tf_student_ 1) = 'true'
        assert (tf_student_ 0) = 'false'
        assert (tf_student_ 'true') = 'false'
)

test_fibonacci =: 3 : 0
    assert (fibonacci_student_ /. (_1 0 1 2 3 4 5 6 7 8 9 10)) = (0 0 1 1 2 3 5 8 13 21 34 55)
)

test_fibIter =: 3 : 0
    assert (fibIter_student_ /. (_1 0 1 2 3 4 5 6 7 8 9 10)) = (0 0 1 1 2 3 5 8 13 21 34 55)
)

test_prime =: 3 : 0
    assert (prime_student_ /. (_1 0 1 2 3 4 5 6 7 8 9 10 99 100 101)) = (0 0 0 1 1 0 1 0 1 0 0 0 0 0 1)
)

test_head =: 3 : 0
    and /
        assert (head_student_ 1 2 3 4 5) = 1
        assert (head_student_ i. 5) = 0
        assert (head_student_ |. i. 5) = 4
)

test_tail =: 3 : 0
    and /
        assert (tail_student_ 1 2 3 4 5) = (2 3 4 5)
        assert (tail_student_ i. 5) = (1 2 3 4)
        assert (tail_student_ |. i. 5) = (3 2 1 0)
)

test_zip =: 3 : 0
    assert (1 2 3 4 zip_student_ 4 3 2 1) = 4 2 $ 1 4 2 3 3 2 4 1
    assert ('abcd' zip_student_ 'hgfe') = 4 2 $ 'ahbgcfde'
)

test_zipBox =: 3 : 0
    assert (1 2 3 4 zipBox_student_ 4 3 2 1) = 4 2 $ ;/ 1 4 2 3 3 2 4 1
    assert (1 2 3 4 zipBox_student_ 'dcba') = 4 2 $ / 1;'d';2;'c';3;'b';4;'a'
)

test_merge =: 3 : 0
    assert (1 3 5 merge_student_ 2 4 6) = 1 2 3 4 5 6
    assert (1 2 6 merge_student_ 2 3 4 5) = 1 2 2 3 4 5 6
)

test_mergesort =: 3 : 0
    perm =: i.@! A. i.
    mapR =: 3 : 'if. 0=#y do. return. else. (/:~~ {. y),(mapR }. y) end.'
    mapS =: 3 : 'if. 0=#y do. return. else. (mergesort_student_ {. y),(mapS }. y) end.'
    and /
        assert (mapS perm 4) = (mapR perm 4)
        assert (mapS perm 5) = (mapR perm 5)
        assert (mapS perm 6) = (mapR perm 6)
        NB. assert (mapS perm 7) = (mapR perm 7)
)

test_parenthesis =: 3 : 0
    and /
        assert (parenthesis_student_ '((()))') = 1 2 3 2 1 0
        assert (parenthesis_student_ '((( )))') = 1 2 3 3 2 1 0
        assert (parenthesis_student_ '((())') = 1 2 3 2 1
        assert (parenthesis_student_ 'hello ( hi (') = 0 0 0 0 0 0 1 1 1 1 1 2
        assert (parenthesis_student_ 'hello ) hi )') = 0 0 0 0 0 0 _1 _1 _1 _1 _1 _2
        assert 0 = _1 { (parenthesis_student_ '(this is ()(() v)alid)')
        assert _1 { (parenthesis_student_ '(this is ()(() invalid)')
        assert (parenthesis_student_ '') = ''
        assert (parenthesis_student_ ' ') = 0
)

NB. run all tests
loc =: > (4 !: 1) 0 1 2 3 NB. get current locale
pre =: 'test_'
succ =: 0
fail =: 0
run_tests =: 3 : 0
    if. (#y) > 0 do.
        f =. {.y
        if. pre -: 5{.f do.
            NB. enable_assert'' NB. ?? can students disable assert inside func?
            display '   running: ',f
            ".('g =: ',f) NB. (deb f) verb trims whitespace (require 'strings')
            r =: 1
            try.
                r =: g''
            catch.
                display 'error occurred'
            end.
            if. r do. fail =: fail + 1
            else. succ =: succ + 1
            end.
        end.
        run_tests (}. y)
    end.
)
NB. output
run_tests loc
display 'You passed';succ;'test(s)'
display 'You failed';fail;'test(s)'
display (fail>0) { ('Success! :)'; 'Failure! :(')



NB. TESTING BREAKDOWN

exit fail NB. output tests as exit code
NB. bash get exit code:  echo $?
