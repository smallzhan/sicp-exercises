(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;; 在计算过程中，我们可以将 flatmap 和 map 分别看作是两重循环。其中 flatmap 是外
;; 层循环，而 map 是内层循环，这样对于 board-size 为 n 的情况，第一个写法的内循环
;; （也就是 (queens-cols (- k 1)) 运行了 n 次，而原来的写法中 (queens-cols (-k
;; 1)) 仅仅运行了一次。这就是问题所在。原来的时间是线性的，而修改版本的时间是指数增长的。

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row k rest-of-queens)
	(enumerate-interval 1 board-size))))
 (queens-cols (- k 1)))

;;这个题目提醒我们的是这样一个道理，在循环过程中，内循环是重复次数最多的高频运行
;;过程也往往是一个程序运行的瓶颈所在，因此在设计嵌套循环的时候我们应该有一个原
;;则，就是让操作开销大的过程尽量的放在外层循环来提高效率。记得以前学习操作系统的
;;时候有个题目关于磁盘寻道的，也是这样，循环过程中， 二维数组的下标变化带来的开销
;;是不同的，一个是另一个的多少倍，此时如果将这个过程放在内循环，那么是非常的浪费
;;时间的。貌似在编译器优化的时候，对一些简单的过程会有一些代码外提的工作，这样可
;;以很好的改进效率。比如在这个题目中，如果在 flatmap 之外有一句 (define old
;;(queens-cols (- k 1))) 然后在下文中出现的地方用 old 代替，就不会出现这样的问题
;;了。