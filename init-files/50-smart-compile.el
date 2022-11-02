;;
;; smart-compile
;;

(with-eval-after-load 'smart-compile
  ;; Brazil CDK projects, kind of
  (add-to-list 'smart-compile-build-system-alist '("\\`cdk.json\\'" . "brazil-build release")))
