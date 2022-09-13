
rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE,
             min.node.size = VARIES!!!!)
