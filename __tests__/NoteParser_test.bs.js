// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var List = require("bs-platform/lib/js/list.js");
var NoteParser$ReactTemplate = require("../src/NoteParser/NoteParser.bs.js");

var cases = /* :: */[
  /* tuple */[
    "",
    ""
  ],
  /* :: */[
    /* tuple */[
      "  ",
      ""
    ],
    /* :: */[
      /* tuple */[
        " a ",
        ""
      ],
      /* :: */[
        /* tuple */[
          "h",
          ""
        ],
        /* :: */[
          /* tuple */[
            "abc",
            ""
          ],
          /* :: */[
            /* tuple */[
              "#",
              ""
            ],
            /* :: */[
              /* tuple */[
                "a",
                "a"
              ],
              /* :: */[
                /* tuple */[
                  "an",
                  "a"
                ],
                /* :: */[
                  /* tuple */[
                    "as",
                    "a#"
                  ],
                  /* :: */[
                    /* tuple */[
                      "a#",
                      "a#"
                    ],
                    /* :: */[
                      /* tuple */[
                        "ab",
                        "ab"
                      ],
                      /* :: */[
                        /* tuple */[
                          "af",
                          "ab"
                        ],
                        /* :: */[
                          /* tuple */[
                            "a+",
                            "a+"
                          ],
                          /* :: */[
                            /* tuple */[
                              "a++",
                              "a++"
                            ],
                            /* :: */[
                              /* tuple */[
                                "a+++",
                                "a+++"
                              ],
                              /* :: */[
                                /* tuple */[
                                  "a-",
                                  "a-"
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "a--",
                                    "a--"
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "a---",
                                      "a---"
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "a0",
                                        "a0"
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "a1",
                                          "a1"
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "a-1",
                                            "a-1"
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "a10",
                                              "a10"
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "as+",
                                                "a#+"
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "as-",
                                                  "a#-"
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "af+++10",
                                                    "ab+++10"
                                                  ],
                                                  /* [] */0
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

describe("NoteParser", (function () {
        return List.iter((function (param) {
                      var expected = param[1];
                      var input = param[0];
                      Jest.test(input, (function () {
                              return Jest.ExpectJs[/* toEqual */12](expected, Jest.ExpectJs[/* expect */0](NoteParser$ReactTemplate.toString(NoteParser$ReactTemplate.fromString(input))));
                            }));
                      return Jest.test(input.toUpperCase(), (function () {
                                    return Jest.ExpectJs[/* toEqual */12](expected, Jest.ExpectJs[/* expect */0](NoteParser$ReactTemplate.toString(NoteParser$ReactTemplate.fromString(input.toUpperCase()))));
                                  }));
                    }), cases);
      }));

var foo = /* () */0;

exports.cases = cases;
exports.foo = foo;
/*  Not a pure module */
