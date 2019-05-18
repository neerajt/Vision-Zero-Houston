snap.points.to.lines <- function(points, lines, join_field=NA, output_type="all"){
  # only accept valid output values
  test_output <- output_type %in% c("all", "closest")
  if(!test_output){
    stop("output_type parameter must take value 'all' or 'closest'")
  }
  
  # only accept valid 
  test_points <- sum(c("x","y","point_id") %in% colnames(points)) == 3
  test_lines <- sum(c("x","y","sequence","line_id") %in% colnames(lines)) == 4
  if(!is.na(join_field)){
    test_points <- test_points & join_field %in% colnames(points)
    test_lines <- test_lines & join_field %in% colnames(lines)
  }
  if(!test_points | !test_lines){
    stop("points data.frame must have 'x', 'y', and 'point_id' fields; lines data.frame must have 'x', 'y', 'sequence', and 'line_id' as fields")
  } else {
    if(is.na(join_field)){
      lines <- lines %>% mutate(join_id = TRUE)
      points <- points %>% mutate(join_id = TRUE)
    } else {
      lines$join_id <- lines[join_field] %>% unlist()
      points$join_id <- points[join_field] %>% unlist()
    }
    
    lines <- lines %>% select(line_id, join_id, sequence, line_x = x, line_y = y)
    points <- points %>% mutate(point_x = x, point_y = y) %>% select(-x, -y)
    segments <- lines %>% 
      transmute(line_id, join_id, sequence_0 = sequence, line_x_0 = line_x, line_y_0 = line_y, sequence_1 = sequence + 1) %>% 
      left_join(lines %>% 
                  select(line_id, sequence_1 = sequence, line_x_1 = line_x, line_y_1 = line_y), 
                by=c("line_id","sequence_1")) %>% 
      filter(!is.na(line_x_1)) %>% 
      mutate(a=line_y_1-line_y_0,
             b=line_x_0-line_x_1) %>% 
      mutate(c=-(a*line_x_0 + b*line_y_0))
    
    # batch 1:
    points_to_vertices <- points %>% 
      left_join(lines, by="join_id") %>% 
      mutate(dist_to_vertex = sqrt((line_x - point_x)^2 + (line_y - point_y)^2))
    
    # batch 2
    points_to_lines <- points %>% 
      left_join(segments, by="join_id") %>% 
      mutate(dist_to_line = abs(a*point_x + b*point_y + c)/sqrt(a^2 + b^2),
             snap_x = (b*(b*point_x - a*point_y) - a*c)/(a^2 + b^2),
             snap_y = (a*(a*point_y - b*point_x) - b*c)/(a^2 + b^2)) %>%
      filter(((line_x_0 <= snap_x & snap_x <= line_x_1)|(line_x_1 <= snap_x & snap_x <= line_x_0)) &
               ((line_y_0 <= snap_y & snap_y <= line_y_1)|(line_y_1 <= snap_y & snap_y <= line_y_0)))
    
    if(output_type == "closest"){
      points_to_vertices <- points_to_vertices %>% 
        group_by(point_id, join_id) %>% 
        filter(dist_to_vertex == min(dist_to_vertex)) %>% 
        ungroup()
      points_to_lines <- points_to_lines %>% 
        group_by(point_id, join_id) %>% 
        filter(dist_to_line == min(dist_to_line)) %>% 
        ungroup()
      points_to_vertices <- points_to_vertices %>% 
        left_join(points_to_lines %>% 
                    group_by(point_id, join_id) %>% 
                    summarise(min_dist_to_line = min(dist_to_line)) %>% 
                    ungroup(), 
                  by=c("point_id", "join_id"))
      points_to_lines <- points_to_lines %>% 
        left_join(points_to_vertices %>%
                    group_by(point_id, join_id) %>% 
                    summarise(min_dist_to_vertex = min(dist_to_vertex)) %>% 
                    ungroup(), 
                  by=c("point_id", "join_id"))
      points_to_vertices <- points_to_vertices %>% 
        filter(is.na(min_dist_to_line) | dist_to_vertex <= min_dist_to_line)
      points_to_lines <- points_to_lines %>% 
        filter(is.na(min_dist_to_vertex) | dist_to_line < min_dist_to_vertex)
    }
    
    list(points_to_vertices=points_to_vertices, 
         points_to_lines=points_to_lines)
  }
}