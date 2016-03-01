let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec);;