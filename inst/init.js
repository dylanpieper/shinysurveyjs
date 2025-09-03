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

        // Clear survey container and ensure it's hidden initially
        $("#surveyContainer").empty().hide();

        let surveyJSON;
        let urlParams = {};
        let expectDbLogicConfig = false;  // New flag to track database logic config expectation

        if (typeof data === "object") {
            surveyJSON = data.survey || data;
            if (data.params) {
                urlParams = data.params;
                console.log("URL parameters:", urlParams);
            }
            // Check if database logic config is expected
            expectDbLogicConfig = !!data.db_logic;
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
                if (paramData?.value !== undefined) {
                    const urlValue = paramData.value;
                    const savedValue = savedData[key];
                    const effectiveSavedValue = savedData[`${key}_param`] !== undefined ?
                        savedData[`${key}_param`] : savedValue;

                    if (urlValue !== effectiveSavedValue) {
                        console.log(`URL param ${key} differs from saved data:`, {
                            url: { value: urlValue, text: paramData.text },
                            saved: { value: effectiveSavedValue, original: savedValue }
                        });
                        shouldResetCookie = true;
                    }
                }
            });
        }

        // Handle data restoration based on URL parameters and saved data
        if (shouldResetCookie) {
            console.log("URL parameters differ from saved data: Resetting cookie");
            deleteCookie(COOKIE_NAME);
            const initialData = {};
            Object.entries(urlParams).forEach(([key, paramData]) => {
                if (paramData?.value !== undefined) {
                    initialData[key] = paramData.text;
                    initialData[`${key}_param`] = paramData.value;
                }
            });
            survey.data = initialData;
            saveSurveyProgress(survey);
        } else if (savedData) {
            survey.data = savedData;
            setTimeout(() => {
                if (savedData._dbLogicConfig) {
                    restoreDbLogicChoices(survey, savedData);
                }
            }, DEBOUNCE_DELAY);
        } else {
            const initialData = {};
            Object.entries(urlParams).forEach(([key, paramData]) => {
                if (paramData?.value !== undefined) {
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
        
                console.log("Raw survey result data:", result.data);
        
                Promise.resolve()
                    .then(() => {
                        const responses = {};
                        for (const [key, value] of Object.entries(result.data)) {
                            if (["data", "currentPageNo", "timestamp"].includes(key)) continue;
                            const question = survey.getQuestionByName(key);
                            if (!question) continue;
        
                            console.log(`Processing field: ${key}`, {
                                type: question.getType(),
                                value: value
                            });
        
                            switch (question.getType()) {
                                case "checkbox":
                                    const baseName = question.name;
                                    const selectedValues = Array.isArray(value) ? value : [value];
                                    
                                    selectedValues.forEach(selectedValue => {
                                        const normalizedKey = `${baseName}..${selectedValue}`;
                                        responses[normalizedKey] = true;
                                    });
                                    break;
        
                                default:
                                    // Check if this question has showOtherItem enabled
                                    if (question.showOtherItem) {
                                        console.log(`Processing field with showOtherItem: ${key}`, {
                                            value: value,
                                            valueType: typeof value,
                                            questionComment: question.comment
                                        });
                                        
                                        // Handle "other" responses separately
                                        // Remove "_id" suffix from key when creating "_other" field
                                        const baseKey = key.endsWith('_id') ? key.slice(0, -3) : key;
                                        const otherKey = `${baseKey}_other`;
                                        
                                        if (typeof value === 'object' && value !== null && 'value' in value && 'other' in value) {
                                            // SurveyJS structure for other responses: {value: selectedChoice, other: otherText}
                                            responses[key] = value.value;
                                            responses[otherKey] = value.other || null;
                                        } else if (value === 'other' && question.comment) {
                                            // Alternative structure where "other" is selected and comment contains the text
                                            responses[key] = value;
                                            responses[otherKey] = question.comment;
                                        } else if (typeof value === 'string' && value.startsWith('other:')) {
                                            // Another possible structure: "other:custom text"
                                            responses[key] = 'other';
                                            responses[otherKey] = value.substring(6); // Remove "other:" prefix
                                        } else if (value === survey.otherValue || (typeof value === 'string' && value === 'other')) {
                                            // User selected "other" option
                                            responses[key] = value;
                                            responses[otherKey] = survey.getOtherValue(key) || null;
                                        } else {
                                            // Regular choice selection (not "other") - don't create _other field at all for non-other responses
                                            responses[key] = question._param ?? value;
                                            // Don't add _other field for regular responses to avoid empty objects
                                        }
                                    } else {
                                        // Regular field without other option
                                        responses[key] = question._param ?? value;
                                    }
                                    break;
                            }
                        }
        
                        console.log("Final processed responses:", responses);
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

        // Initialize survey with proper database logic config handling
        $("#surveyContainer").Survey({
            model: survey,
            onAfterRenderSurvey: () => {
                console.log("Survey rendered, expectDbLogicConfig:", expectDbLogicConfig);
                
                // Set up other field validation after survey is rendered
                if (typeof OtherFieldValidation !== 'undefined') {
                    OtherFieldValidation.init();
                }
                
                if (expectDbLogicConfig) {
                    console.log("Waiting for database logic config");
                    Shiny.setInputValue("surveyReady", true);
                } else {
                    console.log("No database logic config expected, showing survey");
                    document.getElementById("waitingMessage").style.display = "none";
                    document.getElementById("surveyContainer").style.display = "block";
                    Shiny.setInputValue("dbLogicConfigComplete", true);
                }
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