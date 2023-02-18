### Changes in version 0.1.1

#### Bug fixes

-   fixed axis-specific element behavior: now elements that have
    axis-specific children use intersected params from those children.
    Selecting axis now updates element UI controls to reflect
    axis-specific params

-   calc\_element2() had recursion to the original calc\_element()
    function, so relative sizes were calculated if defined in parent
    elements
