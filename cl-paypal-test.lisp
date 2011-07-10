;;
;; CL-PAYPAL - example for a simple hunchentoot paypal server
;;
;; (c) Hans Huebner - hans@huebner.org
;; (c) Manuel Odendahl - wesen@ruinwesen.com
;;
;; To run, call TEST-EXPRESS-CHECKOUT and then point your browser to http://localhost:8080/checkout
;; Go to http://localhost:8080/stop to kill the server.

(eval-when (:compile-toplevel)
  (require :cl-paypal))

;; configuration variables - set these to appropriate values for your account

(defparameter *paypal-api-url*   "https://api-3t.sandbox.paypal.com/nvp")
(defparameter *paypal-user*      "useremail")
(defparameter *paypal-password*  "password")
(defparameter *paypal-signature* "signature")

(setf hunchentoot:*show-lisp-errors-p* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Simple dispatcher
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dispatch-request (request-type request)
  (:documentation "dispatch incoming http request"))

(defmethod no-applicable-method ((function (eql #'dispatch-request)) &rest args)
  (declare (ignore args))
  nil)

(defmacro define-handler (type (request) &body body)
  (let ((request-type-var (gensym)))
    `(defmethod dispatch-request ((,request-type-var (eql ,type)) ,request)
       (declare (ignore ,request-type-var))
       ,@body)))

(defun dispatch-request% (request)
  (let* ((type-string 
          (cl-ppcre:scan-to-strings "[^/]+" (hunchentoot:script-name request)))
         (request-type 
          (and type-string (find-symbol (string-upcase type-string) :keyword))))
    (dispatch-request request-type request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Paypal URLs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-handler :checkout (request)
  (hunchentoot:redirect 
   (cl-paypal:make-express-checkout-url 1 (hunchentoot:remote-addr request))))

(define-handler :stop (request)
  (throw 'stop-server nil))

(define-handler :return-paypal (request)
  (cl-paypal:get-and-do-express-checkout
   (lambda (&key amount currencycode token result) 
     (format t "Paypal Express Checkout OK~%Amount is ~A~%
              Currencycod is ~A~%Token is~A~%Result is ~A"
             amount currencycode token result))
   (lambda () (print "Paypal Express Checkout NG"))))

(define-handler :cancel-paypal (request)
  "Cancelled")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Express checkout initialization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-express-checkout (&key (response-port 8080) (response-host "127.0.0.1"))
  (cl-paypal:init *paypal-api-url*   ; api-url 
                  *paypal-user*      ; user email
                  *paypal-password*  ; password
                  *paypal-signature* ; signature 
                  (format nil "http://~A:~A/return-paypal" response-host response-port) ; return-url 
                  (format nil "http://~A:~A/cancel-paypal" response-host response-port) ; cancel-url
                  :useraction "commit"
                  :currencycode "EUR")
  (catch 'stop-server
    (hunchentoot:start (make-instance 'hunchentoot:acceptor :port response-port
                                      :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
                                      :REQUEST-DISPATCHER  #'dispatch-request%))))
