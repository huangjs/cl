(defvar *dsn-str* "server=157.166.160.75;database=SimAppData;user id=easdbo;pwd=eAsDbO2003;")

(use-package "System.Data.SqlClient")

(defun connect (dsn)
  (let ((connection (SqlConnection.new dsn)))
    (SqlConnection.Open connection)
    connection))

(defmacro with-connection ((ctok dsn) &body body)
  `(let ((,ctok (SqlConnection.new ,dsn)))
     (sqlconnection.open c)
     ,@body
     (sqlconnection.dispose ,ctok)))

    

