(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
    )
  )

(defn spiff [v]
  (+ (or (get v 0) 0) (or (get v 2) 0))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [[x y z]]
    (+ (or x 0) (or z 0))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bl_x bl_y] [tr_x tr_y]]]
  (- tr_x bl_x)
  )

(defn height [[[bl_x bl_y] [tr_x tr_y]]]
  (- tr_y bl_y)
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [[[bl_x bl_y] [tr_x tr_y]] [x y]]
  (and (<= bl_x x tr_x) (<= bl_y y tr_y))
  )

(defn contains-rectangle? [outer [i_lb i_rt]]
  (and (contains-point? outer i_lb) (contains-point? outer i_rt))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (if (contains? author :birth-year)
    (apply str [(:name author) " (" (:birth-year author) " - " (:death-year author) ")"])
    (:name author)
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (== 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
