;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;; A B 两个进程， A 要访问 a， B 要访问 b。
;; 在 A 访问 a 时 B 同时访问了 b。没问题。
;; A 访问 a 时，a 告诉 A 要去操作 b 之后，将得到的结果拿回来使用，
;; B 访问 b 时，b 告诉 B 要先去操作 a...
;; 然后，... 就没有然后了
