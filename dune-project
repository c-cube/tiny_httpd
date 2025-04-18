(lang dune 3.2)
(name tiny_httpd)
(generate_opam_files true)

(authors c-cube)
(maintainers c-cube)
(version 0.19)
(source (github c-cube/tiny_httpd))
(homepage https://github.com/c-cube/tiny_httpd/)
(license MIT)

(package
  (name tiny_httpd)
  (synopsis "Minimal HTTP server using threads")
  (tags (http thread server tiny_httpd http_of_dir simplehttpserver))
  (depopts
    logs
    magic-mime
    (mtime (>= 2.0)))
  (depends
    seq
    base-threads
    result
    hmap
    (iostream (>= 0.2))
    (ocaml (>= 4.08))
    (odoc :with-doc)
    (logs :with-test)
    (conf-libcurl :with-test)
    (ptime :with-test)
    (qcheck-core (and (>= 0.9) :with-test))))

(package
  (name tiny_httpd_camlzip)
  (synopsis "Interface to camlzip for tiny_httpd")
  (depends
    (tiny_httpd (= :version))
    (camlzip (>= 1.06))
    (iostream-camlzip (>= 0.2.1))
    (logs :with-test)
    (odoc :with-doc)))
