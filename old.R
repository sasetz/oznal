
# grab one representative non-missing value for quick variable context
# this helps when scanning large metadata tables in View()

first_non_missing <- function(x) {
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    str_trunc(as.character(x[[1]]), width = 80)
}

# quick reference for all variables, their types, completeness, uniqueness, and schema context
col_profile <- tibble(variable = names(data)) %>%
    mutate(
        class = map_chr(variable, ~ class(data[[.x]])[[1]]),
        non_missing = map_int(variable, ~ sum(!is.na(data[[.x]]) & data[[.x]] != "")),
        non_missing_pct = round(100 * non_missing / nrow(data), 2),
        n_unique = map_int(variable, ~ n_distinct(data[[.x]], na.rm = TRUE)),
        sample_value = map_chr(variable, ~ first_non_missing(data[[.x]])),
        likely_multiselect = map_lgl(variable, ~ any(str_detect(data[[.x]], ";"), na.rm = TRUE)),
        schema_type = schema$type[match(variable, schema$qname)],
        schema_question = schema$question[match(variable, schema$qname)]
    ) %>%
    arrange(non_missing_pct)

# this shows which schema variables are actually present in the public data and their types
schema_coverage <- schema %>%
    select(qname, type, question) %>%
    mutate(in_public_data = qname %in% names(data)) %>%
    arrange(type, desc(in_public_data), qname)

View(col_profile)
View(schema_coverage)

#---- 3. Core distributions for key profile variables ----

plot_top_categories <- function(df, var, top_n = 12) {
    # One tidy frequency table used both for direct inspection and plotting.
    tab <- df %>%
        filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
        count(value = .data[[var]], sort = TRUE) %>%
        mutate(pct = 100 * n / sum(n))
    
    if (nrow(tab) == 0) return(NULL)
    
    p <- tab %>%
        # Top-N keeps country/job labels readable in one figure.
        slice_head(n = top_n) %>%
        mutate(value = fct_reorder(as.factor(value), n)) %>%
        ggplot(aes(x = value, y = n)) +
        geom_col(fill = "#2B6CB0") +
        coord_flip() +
        labs(x = NULL, y = "Count", title = paste("Distribution:", var)) +
        theme_minimal(base_size = 11)
    
    p
}

profile_vars <- c("MainBranch", "Age", "EdLevel", "Employment", "Country", "DevType", "RemoteWork", "JobSat")

dist_results <- map(
    profile_vars,
    ~ plot_top_categories(data, .x, top_n = if_else(.x == "Country", 15L, 12L))
)
names(dist_results) <- profile_vars

dist_results$Country

# save all plots to disk for easy reference
walk(names(dist_results), function(v) {
    p <- dist_results[[v]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("dist_", v, ".png")),
        plot = p,
        width = 9,
        height = 6,
        dpi = 140
    )
})

#---- 4. Numeric-like variables: robust summaries + trimmed histograms ----

numeric_candidates <- c(
    "WorkExp", "YearsCode", "ToolCountWork", "ToolCountPersonal",
    "CompTotal", "ConvertedCompYearly"
)

# Parse numeric fields defensively because many survey TE columns are strings.
# parse_number handles values like "10 years" / currency symbols more robustly.

data_num <- data %>%
    mutate(across(all_of(numeric_candidates), ~ parse_number(as.character(.x))))

numeric_summary <- tibble(variable = numeric_candidates) %>%
    mutate(
        n = map_int(variable, ~ sum(!is.na(data_num[[.x]]))),
        missing_pct = round(100 * (1 - n / nrow(data_num)), 2),
        # Moments + quantiles give a good first pass before formal modeling.
        mean = map_dbl(variable, ~ mean(data_num[[.x]], na.rm = TRUE)),
        sd = map_dbl(variable, ~ sd(data_num[[.x]], na.rm = TRUE)),
        p01 = map_dbl(variable, ~ quantile(data_num[[.x]], 0.01, na.rm = TRUE)),
        p25 = map_dbl(variable, ~ quantile(data_num[[.x]], 0.25, na.rm = TRUE)),
        p50 = map_dbl(variable, ~ quantile(data_num[[.x]], 0.50, na.rm = TRUE)),
        p75 = map_dbl(variable, ~ quantile(data_num[[.x]], 0.75, na.rm = TRUE)),
        p99 = map_dbl(variable, ~ quantile(data_num[[.x]], 0.99, na.rm = TRUE))
    )

plot_numeric <- function(df, var) {
    v <- df[[var]]
    v <- v[!is.na(v)]
    if (length(v) < 10) return(NULL)
    
    upper <- as.numeric(quantile(v, 0.99, na.rm = TRUE))
    # Trim top 1% for visualization only (table keeps full distribution summary).
    tibble(value = v) %>%
        filter(value <= upper) %>%
        ggplot(aes(x = value)) +
        geom_histogram(bins = 35, fill = "#38A169", color = "white") +
        labs(
            x = var,
            y = "Count",
            title = paste("Histogram (trimmed @99th pct):", var)
        ) +
        theme_minimal(base_size = 11)
}

numeric_plots <- map(numeric_candidates, ~ plot_numeric(data_num, .x))
names(numeric_plots) <- numeric_candidates

View(numeric_summary)

walk(names(numeric_plots), function(v) {
    p <- numeric_plots[[v]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("hist_", v, ".png")),
        plot = p,
        width = 8,
        height = 5,
        dpi = 140
    )
})

#---- 4b. Yearly compensation by employment category ----

# we'll be looking primarily at the ConvertedCompYearly field for compensation,
# which is already cleaned version of CompTotal adjusted for the currency.
#
# the assumption is that compensation distributions will differ significantly by
# employment type, but also it can help identify and alleviate general outliers
# like students with low CompTotal

comp_source <- "ConvertedCompYearly"

comp_by_employment <- data %>%
    mutate(
        .keep = "none",
        employment = as.character(Employment),
        yearly_comp = parse_number(as.character(.data[[comp_source]]))
    ) %>%
    filter(!is.na(employment), employment != "", !is.na(yearly_comp), yearly_comp > 0)

top_employment <- comp_by_employment %>%
    count(employment, sort = TRUE) %>%
    slice_head(n = 8)

comp_by_employment <- comp_by_employment %>%
    semi_join(top_employment, by = "employment")

comp_cutoff <- comp_by_employment %>%
    summarise(cutoff = quantile(yearly_comp, 0.99, na.rm = TRUE)) %>%
    pull(cutoff)

plot_comp_by_employment <- comp_by_employment %>%
    filter(yearly_comp <= comp_cutoff) %>%
    ggplot(aes(x = yearly_comp)) +
    geom_histogram(bins = 30, fill = "#4C6EF5", color = "white") +
    facet_wrap(~ employment, ncol = 4) +
    labs(
        title = "Yearly compensation by employment category",
        subtitle = paste("Source:", comp_source, ", trimmed at 99th percentile"),
        x = "Yearly compensation",
        y = "Count"
    ) +
    theme_minimal(base_size = 11)

comp_employment_summary <- comp_by_employment %>%
    group_by(employment) %>%
    summarise(
        n = n(),
        median_comp = median(yearly_comp, na.rm = TRUE),
        mean_comp = mean(yearly_comp, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(median_comp)

plot_comp_by_employment

ggsave(
    filename = file.path("output", "plots", "comp_by_employment_hist_facets.png"),
    plot = plot_comp_by_employment,
    width = 14,
    height = 8,
    dpi = 140
)

#---- 5. Multi-select columns: split and aggregate ----

split_multiselect <- function(df, var, top_n = 15) {
    # convert semicolon-delimited answers into long format for counting options
    out <- df %>%
        transmute(item = .data[[var]]) %>%
        filter(!is.na(item), item != "") %>%
        separate_rows(item, sep = ";\\s*") %>%
        mutate(item = str_squish(item)) %>%
        filter(item != "") %>%
        count(item, sort = TRUE) %>%
        mutate(pct_of_respondents = round(100 * n / nrow(df), 2))
    
    if (nrow(out) == 0) return(NULL)
    
    p <- out %>%
        # top options only so text labels remain interpretable
        slice_head(n = top_n) %>%
        mutate(item = fct_reorder(item, n)) %>%
        ggplot(aes(x = item, y = n)) +
        geom_col(fill = "#DD6B20") +
        coord_flip() +
        labs(x = NULL, y = "Mentions", title = paste("Top multi-select options:", var)) +
        theme_minimal(base_size = 11)
    
    p
}

multiselect_vars <- c(
    "LearnCode", "EmploymentAddl", "AIFrustration", "SO_Dev_Content"
)

# curated variables: enough breadth for a start, without noisy text-entry columns

multi_results <- map(multiselect_vars, ~ split_multiselect(data, .x))
names(multi_results) <- multiselect_vars

walk(names(multi_results), function(v) {
    p <- multi_results[[v]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("multi_", v, ".png")),
        plot = p,
        width = 9,
        height = 6,
        dpi = 140
    )
})

#---- 6. Ranked blocks: average rank + sample-size ----

summarize_rank_block <- function(df, prefix) {
    cols <- names(df)[str_detect(names(df), paste0("^", prefix, "\\d+$"))]
    if (length(cols) == 0) return(NULL)
    
    # ranked columns are stored wide (one column per item), so pivot to long
    rank_long <- df %>%
        select(all_of(cols)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
        mutate(rank = parse_number(as.character(rank))) %>%
        filter(!is.na(rank))
    
    if (nrow(rank_long) == 0) return(NULL)
    
    rank_summary <- rank_long %>%
        group_by(variable) %>%
        summarise(
            # n_ranked helps assess stability of mean rank across items.
            n_ranked = n(),
            mean_rank = mean(rank),
            median_rank = median(rank),
            .groups = "drop"
        ) %>%
        left_join(schema %>% select(qname, sub), by = c("variable" = "qname")) %>%
        arrange(mean_rank)
    
    p <- rank_summary %>%
        slice_head(n = 15) %>%
        # If schema sub-label exists, show that instead of technical qname.
        mutate(label = if_else(is.na(sub), variable, sub)) %>%
        mutate(label = fct_reorder(label, mean_rank, .desc = TRUE)) %>%
        ggplot(aes(x = label, y = mean_rank)) +
        geom_col(fill = "#805AD5") +
        coord_flip() +
        labs(
            x = NULL,
            y = "Average rank (lower is better)",
            title = paste("Top ranked items:", prefix)
        ) +
        theme_minimal(base_size = 11)
    
    p
}

rank_prefixes <- c("TechEndorse_", "TechOppose_", "JobSatPoints_", "SO_Actions_")
rank_results <- map(rank_prefixes, ~ summarize_rank_block(data, .x))
names(rank_results) <- rank_prefixes

walk(names(rank_results), function(prefix) {
    p <- rank_results[[prefix]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("rank_", prefix, ".png")),
        plot = p,
        width = 10,
        height = 6,
        dpi = 140
    )
})

#---- 7. A few bivariate starter views ----

plot_box_if_exists <- function(df, num_var, cat_var) {
    if (!all(c(num_var, cat_var) %in% names(df))) return(NULL)
    
    pdat <- df %>%
        transmute(
            num = parse_number(as.character(.data[[num_var]])),
            cat = as.character(.data[[cat_var]])
        ) %>%
        filter(!is.na(num), !is.na(cat), cat != "")
    
    if (nrow(pdat) < 50) return(NULL)
    
    # Restrict to top levels for categorical readability.
    top_levels <- pdat %>% count(cat, sort = TRUE) %>% slice_head(n = 10) %>% pull(cat)
    pdat <- pdat %>% filter(cat %in% top_levels)
    
    ggplot(pdat, aes(x = fct_reorder(cat, num, median), y = num)) +
        geom_boxplot(fill = "#319795", outlier.alpha = 0.2) +
        coord_flip() +
        labs(x = cat_var, y = num_var, title = paste(num_var, "by", cat_var)) +
        theme_minimal(base_size = 11)
}

bivariate_plots <- list(
    # Starter relationships: useful candidates for your first hypothesis checks.
    years_by_jobsat = plot_box_if_exists(data, "YearsCode", "JobSat"),
    years_by_remote = plot_box_if_exists(data, "YearsCode", "RemoteWork"),
    tools_by_jobsat = plot_box_if_exists(data, "ToolCountWork", "JobSat")
)

walk(names(bivariate_plots), function(name) {
    p <- bivariate_plots[[name]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("box_", name, ".png")),
        plot = p,
        width = 10,
        height = 6,
        dpi = 140
    )
})

#---- 8. Missingness overview ----

missing_top <- col_profile %>%
    arrange(non_missing_pct) %>%
    mutate(variable = fct_reorder(variable, non_missing_pct)) %>%
    slice_head(n = 30)

# Missingness plot is a fast signal for feature feasibility in later models.

plot_missingness <- ggplot(missing_top, aes(x = variable, y = non_missing_pct)) +
    geom_col(fill = "#E53E3E") +
    coord_flip() +
    labs(
        x = NULL,
        y = "Non-missing %",
        title = "30 least complete variables"
    ) +
    theme_minimal(base_size = 11)

ggsave(
    filename = file.path("output", "plots", "missingness_bottom30.png"),
    plot = plot_missingness,
    width = 10,
    height = 8,
    dpi = 140
)

#---- 9. Lightweight console guide ----

numeric_summary
numeric_plots$YearsCode
multi_results$LearnCode
rank_results[["TechEndorse_"]]

#---- 10. Yearly compensation by other demographics ----

# Same idea as employment status, but compact, one helper for all groupings
build_comp_views <- function(df, group_var, plot_name, top_n = NULL) {
    comp_df <- df %>%
        transmute(
            group = as.character(.data[[group_var]]),
            yearly_comp = parse_number(as.character(ConvertedCompYearly))
        ) %>%
        filter(!is.na(group), group != "", !is.na(yearly_comp), yearly_comp > 0)
    
    if (!is.null(top_n)) {
        comp_df <- comp_df %>%
            semi_join(
                comp_df %>% count(group, sort = TRUE) %>% slice_head(n = top_n),
                by = "group"
            )
    }
    
    cutoff <- comp_df %>%
        summarise(v = quantile(yearly_comp, 0.99, na.rm = TRUE)) %>%
        pull(v)
    
    plot_obj <- comp_df %>%
        filter(yearly_comp <= cutoff) %>%
        ggplot(aes(x = yearly_comp)) +
        geom_histogram(bins = 30, fill = "#5C7CFA", color = "white") +
        facet_wrap(~ group, ncol = 4) +
        labs(
            title = paste("Yearly compensation by", group_var),
            subtitle = "ConvertedCompYearly , trimmed at 99th percentile",
            x = "Yearly compensation",
            y = "Count"
        ) +
        theme_minimal(base_size = 11)
    
    ggsave(
        filename = file.path("output", "plots", plot_name),
        plot = plot_obj,
        width = 14,
        height = 8,
        dpi = 140
    )
    
    plot_obj
}

# YearsCode is numeric, so bucket it before faceting.
data <- data %>%
    mutate(
        YearsCode_num = parse_number(as.character(YearsCode)),
        YearsCode_band = cut(
            YearsCode_num,
            breaks = c(-Inf, 2, 5, 10, 15, 20, Inf),
            labels = c("0-2", "3-5", "6-10", "11-15", "16-20", "21+"),
            right = TRUE
        )
    )

comp_age <- build_comp_views(data, "Age", "comp_by_age_hist_facets.png")
comp_country_top5 <- build_comp_views(data, "Country", "comp_by_country_top5_hist_facets.png", top_n = 5)
comp_education <- build_comp_views(data, "EdLevel", "comp_by_education_hist_facets.png")
comp_yearscode <- build_comp_views(data, "YearsCode_band", "comp_by_yearscode_hist_facets.png")

print(comp_age)
print(comp_country_top5)
print(comp_education)
print(comp_yearscode)


# OLD

# Recipes

rec <- recipe(ConvertedCompYearly ~ ., data = data) %>%
    step_rm(all_of(drop_columns)) %>%
    step_factor2string(all_of(multichoice_columns), all_of(nominal_columns)) %>%
    step_mutate(
        EdLevel        = coalesce(na_if(EdLevel, "Other (please specify):"), "Primary/elementary school"),
        Employment     = replace_na(Employment, "I prefer not to say"),
        DevType        = coalesce(na_if(DevType, "Other (please specify):"), "I prefer not to say"),
        ICorPM         = replace_na(ICorPM, "Individual contributor"),
        RemoteWork     = replace_na(RemoteWork, "I prefer not to say"),
        EmploymentAddl = replace_na(EmploymentAddl, "None") %>% str_replace_all(employment_recode),
        EmploymentAddlPaid = case_when(
            str_detect(EmploymentAddl, "Paid30") ~ 3L,
            str_detect(EmploymentAddl, "Paid20") ~ 2L,
            str_detect(EmploymentAddl, "Paid10") ~ 1L,
            TRUE ~ 0L
        ),
        EmploymentAddlSchool = case_when(
            str_detect(EmploymentAddl, "SchoolFullTime")                  ~ 2L,
            str_detect(EmploymentAddl, "SchoolPartTime")                  ~ 1L,
            TRUE                                                          ~ 0L
        ),
        EmploymentAddl = EmploymentAddl %>%
            str_remove_all("Paid\\w+|School\\w+") %>%
            str_remove_all("^;+|;+$") %>%
            str_replace_all(";{2,}", ";")
    ) %>%
    step_string2factor(all_of(nominal_columns)) %>%
    step_unknown(Currency, TechEndorseIntro, new_level = "Missing") %>%
    step_impute_mode(Industry, Country) %>%
    step_dummy(all_of(nominal_columns)) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    View()
#    step_mutate(
#        EmploymentAddl_None = if_else(
#            EmploymentAddl_None == 1 &
#                (EmploymentAddl_Caring == 1 | EmploymentAddl_Retirement == 1 |
#                     EmploymentAddl_Volunteering == 1 | EmploymentAddlPaid > 0 | EmploymentAddlSchool > 0),
#            0L, EmploymentAddl_None
#        )
#    )



##

# MULTICHOICE COLUMNS
# ---

# Let's clean EmploymentAddl. We'll split the original value by semi-colon, then
# create variables with prefix EmploymentAddl for each distinct value.
# For additional paid jobs and school, let's create ordinal variables, since
# they have a clear order (full-time > part-time > none, same for paid 30/20/10
# hours). Missing values are filled with "None of the above", a variable for
# none of the options is also added

# Explore all options
data %>%
    select(EmploymentAddl) %>%
    filter(!is.na(EmploymentAddl)) %>%
    separate_rows(EmploymentAddl, sep = ";\\s*") %>%
    group_by(EmploymentAddl) %>%
    summarise(count = n()) %>%
    arrange(EmploymentAddl) %>%
    distinct() %>%
    View()


employment_recode <- c(
    "Attending school \\(full-time\\)" = "SchoolFullTime",
    "Attending school \\(part-time\\)" = "SchoolPartTime",
    "Caring for dependents \\(children, elderly, etc\\.\\)" = "Caring",
    "Engaged in paid work \\(10-19 hours per week\\)" = "Paid20",
    "Engaged in paid work \\(20-29 hours per week\\)" = "Paid30",
    "Engaged in paid work \\(less than 10 hours per week\\)" = "Paid10",
    "None of the above" = "None",
    "Transitioning to retirement \\(gradually reducing work hours\\)" = "Retirement",
    "Volunteering \\(regularly\\)" = "Volunteering"
)
data %>%
    mutate(
        EmploymentAddl = replace_na(EmploymentAddl, "None") %>% str_replace_all(employment_recode),
        EmploymentAddlPaid = case_when(
            str_detect(EmploymentAddl, "Paid30") ~ 3L,
            str_detect(EmploymentAddl, "Paid20") ~ 2L,
            str_detect(EmploymentAddl, "Paid10") ~ 1L,
            TRUE ~ 0L
        ),
        EmploymentAddlSchool = case_when(
            str_detect(EmploymentAddl, "SchoolFullTime")                  ~ 2L,
            str_detect(EmploymentAddl, "SchoolPartTime")                  ~ 1L,
            TRUE                                                          ~ 0L
        ),
        EmploymentAddl = EmploymentAddl %>%
            str_remove_all("Paid\\w+|School\\w+") %>%
            str_remove_all("^;+|;+$") %>%
            str_replace_all(";{2,}", ";"),
        EmploymentAddl = if_else(EmploymentAddl == "None" | EmploymentAddl == "", "None", EmploymentAddl)
    ) %>%
    relocate(starts_with("EmploymentAddl"), .after = Employment) %>%
    
    # New variable names
    employment_addl_names <- c("SchoolFullTime",
                               "SchoolPartTime",
                               "Caring",
                               "Paid20",
                               "Paid30",
                               "Paid10",
                               "None",
                               "Retirement",
                               "Volunteering")

# Split, pivot, and create ordinal variables for paid hours and school attendance
data <- data %>%
    mutate(EmploymentAddl = if_else(is.na(EmploymentAddl), "None of the above", EmploymentAddl)) %>%
    mutate(row_id = row_number()) %>%
    select(row_id, everything()) %>%
    separate_longer_delim(all_of("EmploymentAddl"), delim = ";") %>%
    mutate(selected = 1) %>%
    mutate(
        EmploymentAddl = factor(EmploymentAddl),
        EmploymentAddl = factor(EmploymentAddl, labels = employment_addl_names),
        EmploymentAddl = paste0("EmploymentAddl", EmploymentAddl)
    ) %>%
    pivot_wider(
        names_from = EmploymentAddl,
        values_from = selected,
        values_fill = 0
    ) %>%
    mutate(
        EmploymentAddlPaid = case_when(
            EmploymentAddlPaid30 == 1 ~ 2,
            EmploymentAddlPaid20 == 1 | EmploymentAddlPaid10 == 1 ~ 1,
            TRUE ~ 0
        )
    ) %>%
    select(!c(EmploymentAddlPaid10, EmploymentAddlPaid20, EmploymentAddlPaid30)) %>%
    mutate(
        EmploymentAddlSchool = case_when(
            EmploymentAddlSchoolFullTime == 1 ~ 2,
            EmploymentAddlSchoolPartTime == 1 ~ 1,
            TRUE ~ 0
        )
    ) %>%
    select(!c(EmploymentAddlSchoolFullTime, EmploymentAddlSchoolPartTime)) %>%
    select(!row_id) %>%
    relocate(starts_with("EmploymentAddl"), .after = Employment)

# Just to confirm that if "none" is 1, no other option is selected
data %>%
    filter(EmploymentAddlNone == 1.) %>%
    select(-EmploymentAddlNone) %>%
    filter(if_any(starts_with("EmploymentAddl"), ~ .x != 0.)) %>%
    select(starts_with("EmploymentAddl")) %>%
    View()

# since we see some data, we have to clean that, too
data <- data %>%
    mutate(EmploymentAddlNone =
               if_else(EmploymentAddlNone == 1 &
                           rowSums(select(., starts_with("EmploymentAddl"), -EmploymentAddlNone)) > 0,
                       0, EmploymentAddlNone)
    )



# check for entries that have platformchoice == no and any other columns that
# starts with Platform as non-NA
data %>%
    filter(PlatformChoice == "No") %>%
    select(-PlatformChoice) %>%
    filter(if_any(starts_with("Platform"), ~ !is.na(.x))) %>%
    select(starts_with("Platform")) %>%
    View()