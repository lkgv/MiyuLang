(lang dune 2.5)
(using menhir 2.1)
(generate_opam_files true)

(name miyu)
(version dev)
(authors "CODERGWY")
(license BSD2)
(maintainers "codergwy@yahoo.com")

(package
 (name miyu)
 (synopsis "A programming language just for practice")
 (depends
  menhir
  (ocaml (>= 4.08.1))))
