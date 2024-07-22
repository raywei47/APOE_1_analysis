WMH_APOE_draft
================
Rachel LeMay
2024-07-20

``` r
setwd("/Users/rachellemay/Desktop/star_u_analyses/APOE_1_analysis")
wmh_dat <- read.csv("/Users/rachellemay/Desktop/star_u_analyses/ABCDS_APOE_df.csv")
```

head(wmh_dat) str(wmh_dat)

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:psych':
    ## 
    ##     logit
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(effectsize)
```

    ## 
    ## Attaching package: 'effectsize'
    ## 
    ## The following object is masked from 'package:psych':
    ## 
    ##     phi

``` r
options(max.print = 10000)
```

``` r
dplyr::filter(wmh_dat, grepl('E4', allele_combo), !grepl('E2/E4', allele_combo), !grepl('E4/E2', allele_combo))
```

    ##    DummyID Frontal_lobe Temporal_lobe Parietal_lobe Occipital_lobe Sum.ROI bl
    ## 1        8        0.032         0.032         0.040          0.008    0.11  2
    ## 2        9        0.040         0.016         0.000          0.024    0.08  0
    ## 3       16        0.056         0.160         0.024          0.656    0.90  2
    ## 4       18        0.056         0.032         0.000          0.000    0.00  0
    ## 5       23        0.064         0.008         0.000          0.000    0.00  0
    ## 6       36        0.096         0.032         0.000          0.008    0.14  0
    ## 7       37        0.096         0.776         0.112          0.184    1.17  0
    ## 8       49        0.112         0.040         0.008          0.008    0.17  0
    ## 9       56        0.128         0.000         0.000          0.000    0.13  0
    ## 10      59        0.128         0.008         0.008          0.000    0.00  0
    ## 11      65        0.144         0.008         0.024          0.064    0.24  0
    ## 12      69        0.152         0.064         0.000          0.000    0.22  0
    ## 13      80        0.168         0.048         0.040          0.000    0.26  1
    ## 14      82        0.168         0.040         0.144          0.072    0.00  0
    ## 15      83        0.184         0.032         0.024          0.000    0.24  0
    ## 16      85        0.192         0.024         0.032          0.016    0.26  0
    ## 17      94        0.208         0.008         0.032          0.016    0.26  0
    ## 18     114        0.280         0.080         0.000          0.000    0.36  0
    ## 19     120        0.320         0.016         0.184          0.024    0.54  2
    ## 20     122        0.328         0.504         0.440          0.288    1.56  0
    ## 21     127        0.352         0.080         0.000          0.048    0.48  0
    ## 22     129        0.352         0.072         0.024          0.008    0.46  0
    ## 23     137        0.408         0.080         0.064          0.008    0.56  0
    ## 24     141        0.440         0.048         0.064          0.024    0.58  1
    ## 25     142        0.456         0.112         0.040          0.000    0.61  0
    ## 26     143        0.456         0.064         0.136          0.000    0.66  1
    ## 27     146        0.472         0.104         0.304          1.200    0.00  0
    ## 28     150        0.496         0.544         1.760          2.208    5.01  0
    ## 29     153        0.512         0.176         0.104          0.376    1.17  0
    ## 30     157        0.536         0.032         0.208          0.008    0.78  2
    ## 31     159        0.544         0.408         0.088          0.336    1.38  0
    ## 32     162        0.552         0.632         0.856          0.512    2.55  0
    ## 33     169        0.608         0.104         0.136          1.768    0.00  0
    ## 34     172        0.664         0.168         0.288          0.312    0.00  0
    ## 35     180        0.784         0.104         0.240          0.024    1.15  0
    ## 36     184        0.840         0.072         0.192          1.208    0.00  0
    ## 37     188        0.912         0.680         2.776          1.048    5.42  0
    ## 38     191        0.928         0.128         0.232          0.752    2.04  1
    ## 39     193        0.936         0.480         0.216          0.176    1.81  0
    ## 40     204        1.088         0.248         2.152          2.000    5.49  0
    ## 41     205        1.104         0.208         0.320          0.312    0.00  0
    ## 42     207        1.152         0.088         0.272          0.112    1.62  0
    ## 43     214        1.208         0.264         0.272          0.208    1.95  1
    ## 44     219        1.256         0.624         1.928          2.000    5.81  0
    ## 45     220        1.256         0.776         9.448          0.056    0.01  0
    ## 46     226        1.360         0.296         0.552          0.328    2.54  0
    ## 47     230        1.424         1.040         0.328          1.288    4.08  0
    ## 48     237        1.520         0.496         2.984          1.088    6.09  0
    ## 49     241        1.632         1.360         0.608          2.736    6.34  0
    ## 50     258        2.176         0.240         2.200          2.472    7.09  1
    ## 51     261        2.264         0.776         2.024          2.608    7.67  0
    ## 52     262        2.272         0.736         0.872          2.240    6.12  0
    ## 53     266        2.368         0.656         2.328          3.496    8.85  0
    ## 54     269        2.496         0.280         1.616          3.656    0.01  0
    ## 55     272        2.592         2.352        11.416          4.816   21.18  2
    ## 56     275        2.672         0.208         1.784          1.392    6.06  3
    ## 57     277        2.880         1.408         1.736          2.352    8.38  2
    ## 58     279        3.080         0.656         3.880          1.088    8.70  1
    ## 59     280        3.112         0.248         0.904          2.888    7.15  1
    ## 60     281        3.200         0.584         3.880          3.984   11.65  2
    ## 61     284        3.384         1.072         7.432          2.264   14.15  1
    ## 62     296        4.096         0.376         3.776          0.696    8.94  0
    ## 63     301        4.336         2.128         8.872          4.664   20.00  2
    ## 64     302        4.384         0.872         2.040          2.680    0.01  0
    ## 65     305        4.864         0.032         0.048          0.000    4.94  0
    ## 66     307        5.008         0.664         3.560          2.904   12.14  1
    ## 67     310        5.440         4.960        14.032          3.680   28.11  1
    ## 68     322       27.656         0.080         3.880          0.672   32.29  2
    ##    c2 c3  X site_id.x allele_combo ds_vs_control_flag.x hh_activity_1
    ## 1   2  2 NA        36        E3/E4                   DS             3
    ## 2   0  0 NA        53        E4/E3                   DS             4
    ## 3   2 NA NA        36        E3/E4                   DS             1
    ## 4  NA NA NA        NA        E3/E4                   DS              
    ## 5  NA NA NA        NA        E3/E4                   DS              
    ## 6   0 NA NA        66        E4/E4                   DS             4
    ## 7   0  0 NA        42        E3/E4                   DS             4
    ## 8   0 NA NA        53        E4/E3                   DS           2|4
    ## 9  NA NA NA        53        E4/E3                   DS             4
    ## 10 NA NA NA        NA        E3/E4                   DS              
    ## 11  0  0 NA        25        E3/E4                   DS           1|4
    ## 12  0 NA NA        66        E3/E4                   DS             3
    ## 13  0 NA NA        42        E3/E4                   DS           1|4
    ## 14 NA NA NA        NA        E3/E4                   DS              
    ## 15  0 NA NA        42        E3/E4                   DS             3
    ## 16  0  0  0        53        E4/E3                   DS             4
    ## 17  0  0 NA        53        E4/E3                   DS             4
    ## 18  0  0 NA        53        E3/E4                   DS           1|4
    ## 19  2  2 NA        42        E3/E4                   DS             1
    ## 20  0  0 NA        92        E4/E4                   DS             2
    ## 21  0  0 NA        53        E3/E4                   DS             4
    ## 22  0  0 NA        36        E3/E4                   DS             2
    ## 23  0  0 NA        36        E3/E4                   DS           1|4
    ## 24  1  1 NA        42        E3/E4                   DS             1
    ## 25  0  3 NA        42        E3/E4                   DS         1|2|4
    ## 26  1 NA NA        42        E3/E4                   DS             1
    ## 27 NA NA NA        NA        E3/E4                   DS              
    ## 28 NA NA NA        92        E4/E3                   DS             4
    ## 29  0  0 NA        25        E3/E4                   DS              
    ## 30  2 NA NA        25        E3/E4                   DS             1
    ## 31  0  0 NA        36        E3/E4                   DS             4
    ## 32  0  0 NA        92        E3/E4                   DS             1
    ## 33 NA NA NA        NA        E3/E4                   DS              
    ## 34 NA NA NA        NA        E3/E4                   DS              
    ## 35  0  1 NA        25        E3/E4                   DS         2|3|4
    ## 36 NA NA NA        NA        E3/E4                   DS              
    ## 37  0  0 NA        25        E3/E4                   DS             3
    ## 38  1  1 NA        92        E3/E4                   DS             4
    ## 39  0  0 NA        36        E3/E4                   DS             2
    ## 40  1  1 NA        36        E3/E4                   DS             2
    ## 41 NA NA NA        NA        E3/E4                   DS              
    ## 42  3 NA NA        53        E3/E4                   DS             2
    ## 43  1  2 NA        42        E3/E4                   DS         1|2|4
    ## 44  0  0 NA        92        E4/E3                   DS             2
    ## 45 NA NA NA        NA        E3/E4                   DS              
    ## 46 NA NA NA        92        E4/E4                   DS             4
    ## 47  0 NA NA        42        E3/E4                   DS             4
    ## 48  0  0 NA        36        E3/E4                   DS             2
    ## 49  0  0 NA        92        E4/E4                   DS             3
    ## 50  2  2 NA        36        E4/E4                   DS           1|2
    ## 51  0  0 NA        92        E4/E3                   DS             4
    ## 52  0  0 NA        42        E3/E4                   DS             1
    ## 53  0  0 NA        92        E3/E4                   DS             4
    ## 54 NA NA NA        NA        E3/E4                   DS              
    ## 55 NA NA NA        36        E3/E4                   DS             1
    ## 56 NA NA NA        74        E4/E3                   DS           1|2
    ## 57  2  2 NA        36        E3/E4                   DS             1
    ## 58  0  0 NA        42        E4/E4                   DS           3|4
    ## 59  2  2 NA        36        E3/E4                   DS             1
    ## 60  2 NA NA        36        E3/E4                   DS         1|2|4
    ## 61  2  2 NA        25        E3/E4                   DS             1
    ## 62  0  0 NA        36        E3/E4                   DS             3
    ## 63  2  2 NA        36        E3/E4                   DS             1
    ## 64 NA NA NA        NA        E3/E4                   DS              
    ## 65  0 NA NA        53        E4/E3                   DS           1|4
    ## 66  2 NA NA        92        E3/E4                   DS             1
    ## 67  2  2 NA        36        E4/E4                   DS           1|2
    ## 68  2  2 NA        42        E3/E4                   DS             1
    ##    hh_activity_job_1 hh_residence_1 de_gender hh_group_home_1 de_ethnicity
    ## 1                 NA              2         1              NA            0
    ## 2                  1              1         2               3            0
    ## 3                 NA              2         1              NA            0
    ## 4                 NA         #NULL!         2              NA            0
    ## 5                 NA         #NULL!         2              NA            0
    ## 6                  2              1         1               3            0
    ## 7                 NA              1         2              NA            0
    ## 8                  3              1         2               1            0
    ## 9                  3              3         1               1            0
    ## 10                NA         #NULL!         2              NA            0
    ## 11                NA              2         2              NA            0
    ## 12                NA              1         1               3            0
    ## 13                NA              3         1              NA            0
    ## 14                NA         #NULL!         2              NA            0
    ## 15                NA              2         1              NA            0
    ## 16                 2              3         2               3            0
    ## 17                 2              1         1               1            0
    ## 18                 3              1         1               1            0
    ## 19                NA              2         1              NA            0
    ## 20                 2              1         1               2            0
    ## 21                 3              1         1               1            0
    ## 22                NA              1         1              NA            0
    ## 23                NA              1         1              NA            1
    ## 24                NA              2         1              NA            0
    ## 25                NA              1         1              NA            0
    ## 26                NA              2         1              NA            0
    ## 27                NA         #NULL!         1              NA            0
    ## 28                 2              1         1               1            0
    ## 29                NA         #NULL!         2              NA            0
    ## 30                NA              2         2              NA            0
    ## 31                NA              2         2              NA            1
    ## 32                 3              2         1               1            0
    ## 33                NA         #NULL!         1              NA            0
    ## 34                NA         #NULL!         1              NA            0
    ## 35                NA              2         1              NA            1
    ## 36                NA         #NULL!         1              NA            0
    ## 37                NA              1         2              NA            0
    ## 38                 2              1         1               1            0
    ## 39                NA              2         2              NA            1
    ## 40                NA              2         1              NA            0
    ## 41                NA         #NULL!         2              NA            1
    ## 42                 3              2         1               2            0
    ## 43                NA              2         1              NA            0
    ## 44                 2              1         2               1            0
    ## 45                NA         #NULL!         2              NA            0
    ## 46                 1              1         2               3            0
    ## 47                NA              2         1              NA            0
    ## 48                NA              2         2              NA            0
    ## 49                NA              1         2               2            0
    ## 50                NA              3         2              NA            0
    ## 51                 2              3         2               1            0
    ## 52                NA              2         1              NA            0
    ## 53                 2              1         2               1            0
    ## 54                NA         #NULL!         1              NA            0
    ## 55                NA              1         2              NA            0
    ## 56                NA              2         1               2            0
    ## 57                NA              2         1              NA            0
    ## 58                NA              2         1              NA            0
    ## 59                NA              1         1              NA            0
    ## 60                NA              2         2              NA            0
    ## 61                NA              1         1              NA            0
    ## 62                NA              1         1              NA            0
    ## 63                NA              2         2              NA            0
    ## 64                NA         #NULL!         2              NA            0
    ## 65                 3              1         1               1            0
    ## 66                 2              2         2               2            0
    ## 67                NA              3         2              NA            0
    ## 68                NA              1         2              NA            0
    ##    de_race de_lang de_lang_spec de_age_mom_birth de_age_dad_birth de_handedness
    ## 1        1       1       #NULL!               27               29             1
    ## 2        1       1       #NULL!               33               39             1
    ## 3        1       1       #NULL!               43               44             1
    ## 4   #NULL!  #NULL!       #NULL!               30               30             1
    ## 5   #NULL!  #NULL!       #NULL!               40               45             1
    ## 6        1       1       #NULL!               34               39             2
    ## 7        1       1       #NULL!               27               25             1
    ## 8        1       1       #NULL!               34               36             2
    ## 9        1       1       #NULL!               23               28             1
    ## 10  #NULL!  #NULL!       #NULL!               35               35             1
    ## 11       2       1       #NULL!               37               36             1
    ## 12       1       1       #NULL!               28               25             1
    ## 13       1       1       #NULL!               26               26             2
    ## 14  #NULL!  #NULL!       #NULL!               22               30             1
    ## 15       1       1       #NULL!               25               26             1
    ## 16       1       1       #NULL!               37               42             1
    ## 17       1       1       #NULL!               29               33             1
    ## 18       1       1       #NULL!               28               29             3
    ## 19       1       1       #NULL!               41               43             1
    ## 20       1       1       #NULL!               31               30             1
    ## 21       1       1       #NULL!               35               36             2
    ## 22       1       1       #NULL!               44               45             1
    ## 23       1       1       #NULL!               18               24             1
    ## 24       1       1       #NULL!               28               28             2
    ## 25       1       1       #NULL!               28               28             1
    ## 26       1       1       #NULL!               39               37             1
    ## 27  #NULL!  #NULL!       #NULL!               29               30             1
    ## 28       1       1       #NULL!               21               24             1
    ## 29       1       1       #NULL!               44               48             1
    ## 30       2       1       #NULL!               28               23             1
    ## 31       1       1       #NULL!               24               24             1
    ## 32       1       1       #NULL!               44               48             1
    ## 33  #NULL!  #NULL!       #NULL!               29               35             1
    ## 34  #NULL!  #NULL!       #NULL!               31               31             3
    ## 35       1       1       #NULL!               31               28             1
    ## 36  #NULL!  #NULL!       #NULL!               30               34             1
    ## 37  #NULL!       1       #NULL!               29               34             2
    ## 38       1       1       #NULL!               23               23             1
    ## 39       1       1       #NULL!               24               24             3
    ## 40       1       1       #NULL!               21               22             2
    ## 41  #NULL!  #NULL!       #NULL!               39               44             1
    ## 42       1       1       #NULL!               28               29             1
    ## 43       1       1       #NULL!               45               48             1
    ## 44       1       1       #NULL!               29               34             1
    ## 45  #NULL!  #NULL!       #NULL!               33               34             1
    ## 46       1       1       #NULL!               27               27             3
    ## 47       1       1       #NULL!               27               28             1
    ## 48       1       1       #NULL!               38               42             1
    ## 49       1       1       #NULL!               27               27             1
    ## 50       1       1       #NULL!               28               28             1
    ## 51       1       1       #NULL!               32               31             1
    ## 52       1       1       #NULL!           #NULL!               NA             1
    ## 53       1       1       #NULL!               28               27             1
    ## 54  #NULL!  #NULL!       #NULL!               37               36             1
    ## 55       1       1       #NULL!               28               30             3
    ## 56       1       1       #NULL!           #NULL!               NA             1
    ## 57       1       1       #NULL!               41               52             1
    ## 58       1       1       #NULL!               43               44             1
    ## 59       1       1       #NULL!               44               43             2
    ## 60       1       1       #NULL!               46               53             1
    ## 61       1       1       #NULL!           #NULL!               NA             2
    ## 62       1       1       #NULL!               30               30             1
    ## 63       1       1       #NULL!               29               34             1
    ## 64  #NULL!  #NULL!       #NULL!               29               29             3
    ## 65       1       1       #NULL!               28               35             2
    ## 66       1       1       #NULL!               27               29             1
    ## 67       1       1       #NULL!               28               28             1
    ## 68       1       1       #NULL!               40               45             1
    ##    Sum.of.age_at_visit Sum.of.age_at_visit.1 Sum.of.age_at_visit.2
    ## 1                   65                    67                    68
    ## 2                   28                    29                    31
    ## 3                   60                    62                    NA
    ## 4                   29                    NA                    NA
    ## 5                   39                    NA                    NA
    ## 6                   34                    35                    NA
    ## 7                   40                    41                    42
    ## 8                   31                    33                    NA
    ## 9                   36                    NA                    NA
    ## 10                  36                    NA                    NA
    ## 11                  41                    42                    44
    ## 12                  29                    30                    NA
    ## 13                  49                    51                    NA
    ## 14                  53                    NA                    NA
    ## 15                  47                    48                    NA
    ## 16                  38                    40                    41
    ## 17                  32                    33                    34
    ## 18                  26                    28                    29
    ## 19                  62                    63                    65
    ## 20                  33                    34                    36
    ## 21                  35                    36                    37
    ## 22                  40                    41                    43
    ## 23                  41                    42                    44
    ## 24                  49                    50                    52
    ## 25                  43                    44                    46
    ## 26                  52                    53                    NA
    ## 27                  37                    NA                    NA
    ## 28                  27                    NA                    NA
    ## 29                  46                    47                    50
    ## 30                  56                    58                    NA
    ## 31                  40                    41                    44
    ## 32                  49                    51                    52
    ## 33                  39                    NA                    NA
    ## 34                  29                    NA                    NA
    ## 35                  49                    50                    52
    ## 36                  34                    NA                    NA
    ## 37                  47                    49                    51
    ## 38                  43                    45                    47
    ## 39                  40                    41                    44
    ## 40                  45                    47                    48
    ## 41                  25                    NA                    NA
    ## 42                  57                    59                    NA
    ## 43                  59                    60                    61
    ## 44                  28                    29                    31
    ## 45                  41                    NA                    NA
    ## 46                  39                    NA                    NA
    ## 47                  41                    42                    NA
    ## 48                  49                    50                    51
    ## 49                  35                    36                    37
    ## 50                  45                    47                    49
    ## 51                  37                    39                    41
    ## 52                  66                    67                    68
    ## 53                  35                    37                    39
    ## 54                  29                    NA                    NA
    ## 55                  55                    NA                    NA
    ## 56                  56                    NA                    NA
    ## 57                  54                    55                    57
    ## 58                  48                    49                    51
    ## 59                  56                    57                    59
    ## 60                  59                    60                    NA
    ## 61                  56                    58                    60
    ## 62                  50                    52                    53
    ## 63                  46                    48                    49
    ## 64                  37                    NA                    NA
    ## 65                  50                    52                    NA
    ## 66                  43                    45                    NA
    ## 67                  45                    47                    49
    ## 68                  54                    56                    57
    ##    Sum.of.age_at_visit.3 site_name event_code.y   e4 filter_.
    ## 1                     NA       UCI           bl 1.00        1
    ## 2                     NA         0           bl 1.00        1
    ## 3                     NA       UCI           bl 1.00        1
    ## 4                     NA      <NA>           bl 1.00        1
    ## 5                     NA      <NA>           bl 1.00        1
    ## 6                     NA         0           bl 1.00        1
    ## 7                     NA       MGH           bl 1.00        1
    ## 8                     NA         0           bl 1.00        1
    ## 9                     NA         0           bl 1.00        1
    ## 10                    NA      <NA>           bl 1.00        1
    ## 11                    NA  Columbia           c2 1.00        1
    ## 12                    NA         0           bl 1.00        1
    ## 13                    NA       MGH           c2 1.00        1
    ## 14                    NA      <NA>           bl 1.00        1
    ## 15                    NA       MGH           c2 1.00        1
    ## 16                    43         0           bl 1.00        1
    ## 17                    NA         0           bl 1.00        1
    ## 18                    NA         0           bl 1.00        1
    ## 19                    NA       MGH           bl 1.00        1
    ## 20                    NA         0           bl 1.00        1
    ## 21                    NA         0           bl 1.00        1
    ## 22                    NA       UCI           c2 1.00        1
    ## 23                    NA       UCI           c2 1.00        1
    ## 24                    NA       MGH           bl 1.00        1
    ## 25                    NA       MGH           bl 1.00        1
    ## 26                    NA       MGH           bl 1.00        1
    ## 27                    NA      <NA>           bl 1.00        1
    ## 28                    NA         0           bl 1.00        1
    ## 29                    NA  Columbia           bl 1.00        1
    ## 30                    NA  Columbia           bl 1.00        1
    ## 31                    NA       UCI           c2 1.00        1
    ## 32                    NA         0           bl 1.00        1
    ## 33                    NA      <NA>           bl 1.00        1
    ## 34                    NA      <NA>           bl 1.00        1
    ## 35                    NA  Columbia           bl 1.00        1
    ## 36                    NA      <NA>           bl 1.00        1
    ## 37                    NA  Columbia           c2 1.00        1
    ## 38                    NA         0           bl 1.00        1
    ## 39                    NA       UCI           c2 1.00        1
    ## 40                    NA       UCI           c2 1.00        1
    ## 41                    NA      <NA>           bl 1.00        1
    ## 42                    NA         0           bl 1.00        1
    ## 43                    NA       MGH           c2 1.00        1
    ## 44                    NA         0           bl 1.00        1
    ## 45                    NA      <NA>           bl 1.00        1
    ## 46                    NA         0           bl 1.00        1
    ## 47                    NA       MGH           bl 1.00        1
    ## 48                    NA       UCI           c2 1.00        1
    ## 49                    NA         0           bl 1.00        1
    ## 50                    NA       UCI           c2 1.00        1
    ## 51                    NA         0           bl 1.00        1
    ## 52                    NA       MGH           c2 1.00        1
    ## 53                    NA         0           bl 1.00        1
    ## 54                    NA      <NA>           bl 1.00        1
    ## 55                    NA       UCI           bl 1.00        1
    ## 56                    NA         0           bl 1.00        0
    ## 57                    NA       UCI           bl 1.00        1
    ## 58                    NA       MGH           bl 1.00        1
    ## 59                    NA       UCI           c2 1.00        1
    ## 60                    NA       UCI           bl 1.00        1
    ## 61                    NA  Columbia           c2 1.00        1
    ## 62                    NA       UCI           c2 1.00        1
    ## 63                    NA       UCI           bl 1.00        1
    ## 64                    NA      <NA>           bl 1.00        1
    ## 65                    NA         0           bl 1.00        1
    ## 66                    NA         0           bl 1.00        1
    ## 67                    NA       UCI           c2 1.00        1
    ## 68                    NA       MGH           bl 1.00        1

``` r
## so there's 72 pple w/ E4 total but excluding those carrying E2 w E4 bc that could confound results
```
