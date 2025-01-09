function updateDynamicChoices(data) {
    const attemptUpdate = (retries = 3) => {
        if (typeof survey === 'undefined' || survey === null) {
            if (retries > 0) {
                console.log(`Waiting for survey... (${retries} attempts left)`);
                setTimeout(() => attemptUpdate(retries - 1), CHOICE_UPDATE_DELAY);
                return;
            }
            document.getElementById("surveyNotDefinedMessage").style.display = "block";
            console.error("Survey not initialized after retries");
            return;
        }

        // Process each field in the data
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            const targetQuestion = survey.getQuestionByName(fieldName);
            if (!targetQuestion) {
                console.warn(`Target question not found: ${fieldName}`);
                return;
            }

            // Set choices if this is a parent or standalone field
            if (fieldData.type === "parent" || !fieldData.type) {
                // Format choices for SurveyJS
                const choices = fieldData.choices;
                let surveyChoices = [];

                // Handle parallel arrays structure
                if (choices.value && Array.isArray(choices.value)) {
                    surveyChoices = choices.value.map((value, index) => ({
                        value: value,
                        text: choices.text[index]
                    }));
                }

                console.log("Setting choices for", fieldName, ":", surveyChoices);
                targetQuestion.choices = surveyChoices;

                // Set up parent change handler
                if (fieldData.type === "parent") {
                    // Store data for access in callbacks
                    survey.parentConfig = survey.parentConfig || {};
                    survey.parentConfig[fieldName] = {
                        data: data,
                        choices: choices
                    };

                    // Remove any existing handler
                    if (survey.onValueChanged) {
                        const existingHandlers = survey.onValueChanged.actions || [];
                        survey.onValueChanged.actions = existingHandlers.filter(
                            handler => handler.name !== `update_${fieldName}_children`
                        );
                    }

                    // Add new value changed handler
                    survey.onValueChanged.add(function(sender, options) {
                        if (options.name !== fieldName) return;

                        console.log("Value changed event for", fieldName, ":", options);
                        const parentValue = options.value;
                        console.log("Parent value is:", parentValue);

                        if (!parentValue) return;

                        const config = survey.parentConfig[fieldName];
                        if (!config) return;

                        // Find the child field and index
                        const parentIndex = config.choices.value.indexOf(parentValue);
                        if (parentIndex === -1) return;

                        const childField = config.choices.childField[parentIndex];
                        console.log("Child field:", childField, "Parent index:", parentIndex);

                        if (childField) {
                            const childQuestion = survey.getQuestionByName(childField);
                            const childData = config.data[childField];
                            console.log("Child data:", childData);

                            if (childQuestion && childData && childData.choices) {
                                const parentId = parentIndex + 1; // Convert to 1-based index
                                console.log("Looking for parent ID:", parentId);

                                let childChoices = [];

                                // Process each value in parallel arrays
                                const childChoicesData = childData.choices;
                                childChoicesData.value.forEach((value, idx) => {
                                    const parentIdForChoice = childChoicesData.parentId[idx];
                                    console.log(`Processing child choice ${idx}:`, {
                                        value,
                                        parentId: parentIdForChoice
                                    });

                                    if (parentIdForChoice === parentId) {
                                        if (Array.isArray(value)) {
                                            // Handle array values
                                            value.forEach((val, valIdx) => {
                                                childChoices.push({
                                                    value: val,
                                                    text: childChoicesData.text[idx][valIdx]
                                                });
                                            });
                                        } else {
                                            childChoices.push({
                                                value: value,
                                                text: childChoicesData.text[idx]
                                            });
                                        }
                                    }
                                });

                                console.log("Setting child choices:", childChoices);
                                childQuestion.choices = childChoices;

                                // Reset child value if not in new choices
                                if (childQuestion.value !== null &&
                                    !childChoices.some(c => c.value === childQuestion.value)) {
                                    childQuestion.value = null;
                                }
                            }
                        }
                    }, `update_${fieldName}_children`);
                }
            }
        });
    };

    attemptUpdate();
}

Shiny.addCustomMessageHandler("updateDynamicChoices", updateDynamicChoices);

function restoreDynamicChoices(survey, savedData) {
    if (!savedData || !savedData._dynamicConfig) return;

    const { parentFields, childChoices } = savedData._dynamicConfig;

    // First restore parent values
    Object.entries(parentFields).forEach(([parentField, config]) => {
        const parentQuestion = survey.getQuestionByName(parentField);
        if (parentQuestion) {
            parentQuestion.value = config.value;
        }
    });

    // Then restore child choices and values
    Object.entries(childChoices).forEach(([childField, config]) => {
        const childQuestion = survey.getQuestionByName(childField);
        if (childQuestion) {
            // Restore the choices
            childQuestion.choices = config.choices;

            // Restore the value if it exists in the choices
            if (config.value !== undefined &&
                config.choices.some(choice => choice.value === config.value)) {
                childQuestion.value = config.value;
            }
        }
    });
}
