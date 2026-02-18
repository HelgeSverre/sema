; Comment injection — enables TODO/FIXME highlighting inside comments.
([(comment) (block_comment)] @content
  (#set! injection.language "comment"))
