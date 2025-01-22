function initializeSurvey(data) {
    try {
        if (!data) {
            throw new Error("No survey data provided");
        }

        console.log("Survey data received:", data);

        // Clear existing survey if it exists
        if (survey) {
            try {
                survey.dispose();
            } catch (e) {
                console.warn("Error disposing existing survey:", e);
            }
            survey = null;
        }

        // Clear survey container
        $("#surveyContainer").empty();

        let surveyJSON;
        let urlParams = {};

        if (typeof data === "object") {
            surveyJSON = data.survey || data;
            if (data.params) {
                urlParams = data.params;
                console.log("URL parameters:", urlParams);
            }
        } else if (typeof data === "string") {
            try {
                surveyJSON = JSON.parse(data);
            } catch (error) {
                throw new Error("Invalid survey JSON format: " + error.message);
            }
        } else {
            throw new Error("Invalid survey data format");
        }

        survey = new Survey.Model(surveyJSON);

        // Get saved data
        const savedData = getCookie(COOKIE_NAME);
        console.log("Saved cookie data:", savedData);

        // Check if URL parameters differ from saved data
        let shouldResetCookie = false;
        if (savedData && Object.keys(urlParams).length > 0) {
            Object.entries(urlParams).forEach(([key, paramData]) => {
                // Only compare if we have a value in the URL parameter
                if (paramData?.value !== undefined) {
                    const urlValue = paramData.value;
                    const savedValue = savedData[key];

                    // Get the saved value, checking both direct value and possible _param
                    const effectiveSavedValue = savedData[`${key}_param`] !== undefined ?
                        savedData[`${key}_param`] : savedValue;

                    if (urlValue !== effectiveSavedValue) {
                        console.log(`URL param ${key} differs from saved data:`, {
                            url: {
                                value: urlValue,
                                text: paramData.text
                            },
                            saved: {
                                value: effectiveSavedValue,
                                original: savedValue
                            }
                        });
                        shouldResetCookie = true;
                    }
                }
            });
        }

        // If URL parameters differ, reset everything
        if (shouldResetCookie) {
            console.log("URL parameters differ from saved data: Resetting cookie");
            deleteCookie(COOKIE_NAME);

            // Set only URL parameters
            const initialData = {};
            Object.entries(urlParams).forEach(([key, paramData]) => {
                if (paramData?.value !== undefined) {
                    // Store both the display text and internal value
                    initialData[key] = paramData.text;
                    initialData[`${key}_param`] = paramData.value;
                }
            });
            survey.data = initialData;

            // Save new state
            saveSurveyProgress(survey);
        } else if (savedData) {
            // No URL parameter changes, restore saved data
            survey.data = savedData;
            setTimeout(() => {
                if (savedData._dynamicConfig) {
                    restoreDynamicChoices(survey, savedData);
                }
            }, DEBOUNCE_DELAY);
        } else {
            // No saved data, just set URL parameters if any
            const initialData = {};
            Object.entries(urlParams).forEach(([key, paramData]) => {
                if (paramData?.value !== undefined) {
                    // Store both the display text and internal value
                    initialData[key] = paramData.text;
                    initialData[`${key}_param`] = paramData.value;
                }
            });
            if (Object.keys(initialData).length > 0) {
                survey.data = initialData;
            }
        }

        // Set up hidden fields
        setHiddenFieldsFromShiny(survey, urlParams);

        // Set up value change handler
        let valueChangeTimeout;
        survey.onValueChanged.add((sender, options) => {
            console.log("Value changed:", options.name, options.value);

            clearTimeout(valueChangeTimeout);
            valueChangeTimeout = setTimeout(() => {
                saveSurveyProgress(survey);
                if (typeof survey.runConditions === "function") {
                    survey.runConditions();
                }
            }, DEBOUNCE_DELAY);
        });

        // Set up completion handler
        survey.onComplete.add((result) => {
            try {
                document.getElementById("savingDataMessage").style.display = "block";
                Shiny.setInputValue("surveyComplete", true);

                Promise.resolve()
                    .then(() => {
                        const responses = {};
                        for (const [key, value] of Object.entries(result.data)) {
                            if (["data", "currentPageNo", "timestamp"].includes(key)) continue;

                            const question = survey.getQuestionByName(key);
                            if (!question) continue;

                            // Handle checkbox/multi-select questions consistently
                            if (question.getType() === "checkbox") {
                                // Get the base name (remove any existing prefixes)
                                const baseName = question.name;

                                // For checkbox questions, value will be an array or single value
                                const selectedValues = Array.isArray(value) ? value : [value];

                                // Create consistent column names for each selected option
                                selectedValues.forEach(selectedValue => {
                                    const normalizedKey = `${baseName}..${selectedValue}`;
                                    responses[normalizedKey] = true;
                                });
                            } else {
                                // For other question types, use the standard handling
                                responses[key] = question._param ?? value;
                            }
                        }
                        return responses;
                    })
                    .then((responses) => {
                        Shiny.setInputValue("surveyData", responses);
                    })
                    .then(() => {
                        deleteCookie(COOKIE_NAME);
                        return new Promise(resolve => {
                            setTimeout(() => {
                                const remainingCookie = getCookie(COOKIE_NAME);
                                if (remainingCookie) {
                                    deleteCookie(COOKIE_NAME);
                                }
                                resolve();
                            }, 100);
                        });
                    })
                    .catch(error => {
                        console.error("Error in completion handler:", error);
                        Shiny.setInputValue("surveyError", {
                            type: "CompletionError",
                            message: "Error processing survey completion",
                            details: error.message
                        });
                    });
            } catch (error) {
                console.error("Error in completion handler:", error);
                Shiny.setInputValue("surveyError", {
                    type: "CompletionError",
                    message: "Error processing survey completion",
                    details: error.message
                });
            }
        });

        // Initialize survey and signal when ready
        $("#surveyContainer").Survey({
            model: survey,
            onAfterRenderSurvey: () => {
                console.log("Loaded survey");
                // Signal that survey is ready for dynamic updates
                Shiny.setInputValue("surveyReady", true);
            }
        });

    } catch (error) {
        console.error("Survey initialization error:", error);
        Shiny.setInputValue("surveyError", {
            type: "InitializationError",
            message: "Failed to initialize survey",
            details: error.message
        });
    }
}
