(lang dune 3.13)
(name chamallaw)
(generate_opam_files true)

(package
 (name chamallaw)
 (synopsis "Standalone law package scaffold for Chamallaw")
 (description
  "Standalone law-domain package scaffold with package-owned schema fragments, typed host adapters, and smoke verification for Chamallaw Epic 89.")
 (depends
  (ocaml (>= 5.3.0))
   (dune (>= 3.13))
   yojson
   ptime
   ppx_blob
   (caqti (= "2.2.4"))
  (caqti-eio (= "2.2.4"))
  (alcotest :with-test)
  (caqti-driver-sqlite3 :with-test)
  (caqti-eio.unix :with-test)
  (eio :with-test)
  (eio_posix :with-test))
 (license "Proprietary")
 (authors "Epure Team")
 (maintainers "Epure Team")
 (homepage "https://github.com/epure-team/epure")
 (bug_reports "https://github.com/epure-team/epure/issues"))
