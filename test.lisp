(in-package nospam)

;; Unit test code taken from Peter Seibel's book Practical Common Lisp:
;; http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html
;; changed liberally to accommodate our needs

(defvar *test-name* nil)
(defvar *test-count* 0)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (incf *test-count*)
  (format t "~:[FAIL~;pass~] ~3d ~a~%" result *test-count* *test-name*)
  (if (null result)
      (format t "failed test:~%~a~%" form))
  result)

;; Here come the tests

(deftest test-qp-decode ()
  (check
   (string=
    "Szenvedsz a nehéz emésztés miatt? 
Már sok módszert kipróbáltál, de nem jártak megfelelő 
eredménnyel? Most részesülhetsz a legjobb megoldásban. 
Lépj be ide, hogy megtudd mit tehetsz ezért.
"
    (qp-decode "Szenvedsz a neh=C3=A9z em=C3=A9szt=C3=A9s miatt?=20
M=C3=A1r sok m=C3=B3dszert kipr=C3=B3b=C3=A1lt=C3=A1l, de nem j=C3=A1rtak megfelel=C5=91=20
eredm=C3=A9nnyel? Most r=C3=A9szes=C3=BClhetsz a legjobb megold=C3=A1sban.=20
L=C3=A9pj be ide, hogy megtudd mit tehetsz ez=C3=A9rt." :utf-8))

   (string=
    "Tisztelt Partnerünk!

November 4 és 7 között ingyenes, angol nyelvű  
oktatást szervezünk, melyre az Ön jelentkezését is várjuk. 
Tekintse meg a mellékelt programot és amennyiben felkelti érdeklődését, 
regisztráljon ezen az oldalon:
Az oktatás tartalmazza a napi 2 kávészünetet és 1 ebédjegyet. 
Felhívjuk továbbá szíves figyelmét, hogy a helyek száma 25 főben 
korlátozott. A helyek betöltése érkezési sorrendben történik.
A jelentkezéseket legkésőbb november 3-ig várjuk!
Üdvözlettel/Kind regards
"
    (qp-decode "Tisztelt Partner=FCnk!

November 4 =E9s 7 k=F6z=F6tt ingyenes, angol nyelv=FB =20
oktat=E1st szervez=FCnk, melyre az =D6n jelentkez=E9s=E9t is v=E1rjuk.=20
Tekintse meg a mell=E9kelt programot =E9s amennyiben felkelti =E9rdekl=F5d=E9s=E9t,=20
regisztr=E1ljon ezen az oldalon:
Az oktat=E1s tartalmazza a napi 2 k=E1v=E9sz=FCnetet =E9s 1 eb=E9djegyet.=20
Felh=EDvjuk tov=E1bb=E1 sz=EDves figyelm=E9t, hogy a helyek sz=E1ma 25 f=F5ben=20
korl=E1tozott. A helyek bet=F6lt=E9se =E9rkez=E9si sorrendben t=F6rt=E9nik.
A jelentkez=E9seket legk=E9s=F5bb november 3-ig v=E1rjuk!
=DCdv=F6zlettel/Kind regards" :iso-8859-2))))

(deftest test-base64-decode ()
  (check
   (string=
    "ik félnek át nem adjuk. Ha levelünkkel zavartuk, akkor"
    (base64-decode "aWsgZsOpbG5layDDoXQgbmVtIGFkanVrLiBIYSBsZXZlbMO8bmtrZWwgemF2YXJ0dWssIGFra29y" :utf-8))
   (string=
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional/"
    (base64-decode "PCFET0NUWVBFIEhUTUwgUFVCTElDICItLy9XM0MvL0RURCBIVE1MIDQuMCBUcmFuc2l0aW9uYWwv" :windows-1252))
   (string=
    "My name is Peter Fong, a personal Accountant/"
    (base64-decode "TXkgbmFtZSBpcyBQZXRlciBGb25nLCBhIHBlcnNvbmFsIEFjY291bnRhbnQv" :us-ascii))))

(deftest test-content-decode ()
  (combine-results
   (test-qp-decode)
   (test-base64-decode)))

(deftest test-decode-html-entities ()
  (check
   (string=
    "és folyamatos leárazások várnak."
    (decode-html-entities "&eacute;s folyamatos le&aacute;raz&aacute;sok v&aacute;rnak."))
   (string=
    "<a href=\"http://bonuszshop.com/hirlevel-2015-02-12/?utm_source=newsletter&utm_medium=bsemail&utm_content=open&utm_campaign=2015-02-12\">Böngészőnézet.</a>"
    (decode-html-entities "<a href=\"http://bonuszshop.com/hirlevel-2015-02-12/?utm_source=newsletter&amp;utm_medium=bsemail&amp;utm_content=open&amp;utm_campaign=2015-02-12\">B&ouml;ng&eacute;szőn&eacute;zet.</a>"))))

(deftest test-mime-dec-qenc ()
  (check
   (string=
    "From: \"Lic. Guadalupe Díaz\""
    (mime-decode-line "From: \"=?utf-8?Q?Lic._Guadalupe_D=C3=ADaz?=\""))
   (string=
    "Subject: Mindenki szeret jó minőségű, márkás ruhákban járni, azonban nem mindenki engedheti meg magának"
    (mime-decode-line "Subject: =?utf-8?Q?Mindenki_szeret_j=C3=B3_min=C5=91s=C3=A9g=C5=B1,_m=C3=A1rk?= =?utf-8?Q?=C3=A1s_ruh=C3=A1kban_j=C3=A1rni,_azonban_nem_mindenki_engedhet?= =?utf-8?Q?i_meg_mag=C3=A1nak?="))))

(deftest test-mime-dec-base64 ()
  (check
   (string=
    "Subject: A tökéletes dekorációs ajándékot keresed?"
    (mime-decode-line "Subject: =?UTF-8?B?QSB0w7Zrw6lsZXRlcyBkZWtvcsOhY2nDs3MgYWrDoW5kw6lrb3Qga2VyZXNlZD8=?="))
   (string=
    "Subject: VEZETŐI  ASSZISZTENSI  ÉS  CÉGASSZISZTENSI TANFOLYAM  //  Munkaköri képzés, továbbképzés  // Pályázati felhívás  //  Új program"
    (mime-decode-line "Subject: =?UTF-8?B?VkVaRVTFkEkgIEFTU1pJU1pURU5TSSAgw4lTICBDw4lHQVNTWklTWlRFTlNJIFRBTkZPTFlBTSAgLy8gIE11bmtha8O2cmkga8OpcHrDqXMsIHRvdsOhYmJrw6lwesOpcyAgLy8gUMOhbHnDoXphdGkgZmVsaMOtdsOhcyAgLy8gIMOaaiBwcm9ncmFt?="))
   (string=
    "From: \"Bónusz Shop\" <info@bonuszshop.com>"
    (mime-decode-line "From: \"=?UTF-8?B?QsOzbnVzeiBTaG9w?=\" <info@bonuszshop.com>"))))

(deftest test-mime-decode-line ()
  (combine-results
   (test-mime-dec-qenc)
   (test-mime-dec-base64)))

(deftest test-nospam ()
  (combine-results
   (test-content-decode)
   (test-decode-html-entities)
   (test-mime-decode-line)))
