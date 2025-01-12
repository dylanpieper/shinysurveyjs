function updateDynamicChoices(data) {
    // Store metrics for summary
    const metrics = {
        processedFields: {
            standalone: 0,
            child: 0,
            paramParent: 0,
            choiceParent: 0
        },
        updatedChoices: 0,
        errors: [],
        warnings: []
    };

    function updateChildFieldsFromParents() {
        if (!survey.parentConfig) {
            metrics.warnings.push("No parent config found");
            return;
        }

        Object.entries(survey.parentConfig).forEach(([parentField, config]) => {
            const parentQuestion = survey.getQuestionByName(parentField);
            if (parentQuestion && parentQuestion.value) {
                updateChildFields(parentField, parentQuestion.value);
            }
        });
    }

    function processChildField(fieldName, fieldData) {
        survey.childConfig = survey.childConfig || {};
        survey.childConfig[fieldName] = fieldData;
        metrics.processedFields.child++;
    }

    function setChoices(question, choicesData) {
        if (!choicesData || !Array.isArray(choicesData.value)) {
            metrics.warnings.push(`Invalid choices data for ${question.name}`);
            return;
        }

        const surveyChoices = choicesData.value.map((value, index) => ({
            value: value,
            text: choicesData.text[index]
        }));

        question.choices = surveyChoices;
        metrics.updatedChoices++;
    }

    function setupParentHandler(fieldName, fieldData) {
        survey.parentConfig = survey.parentConfig || {};
        survey.parentConfig[fieldName] = {
            data: data,
            childField: fieldData.childField,
            fieldData: fieldData,
            type: fieldData.type,
            valueToIdMap: fieldData.choices.value.reduce((map, value, index) => {
                map[value] = fieldData.choices.ids[index];
                return map;
            }, {}),
            choicesList: fieldData.choices
        };

        if (survey.onValueChanged) {
            const existingHandlers = survey.onValueChanged.actions || [];
            survey.onValueChanged.actions = existingHandlers.filter(
                handler => handler.name !== `update_${fieldName}_children`
            );
        }

        const handlerFunction = function(sender, options) {
            if (options.name !== fieldName) return;
            updateChildFields(fieldName, options.value);
            saveSurveyProgress(survey);
        };

        survey.onValueChanged.add(handlerFunction, `update_${fieldName}_children`);

        const currentValue = survey.getQuestionByName(fieldName).value;
        if (currentValue) {
            updateChildFields(fieldName, currentValue);
        }

        if (fieldData.type === 'param_parent') {
            metrics.processedFields.paramParent++;
        } else if (fieldData.type === 'choice_parent') {
            metrics.processedFields.choiceParent++;
        }
    }

    function updateChildFields(parentField, parentValue) {
        if (!parentValue) return;

        const parentConfig = survey.parentConfig[parentField];
        if (!parentConfig) {
            metrics.warnings.push(`No parent config found for ${parentField}`);
            return;
        }

        const childField = parentConfig.childField;
        if (!childField) {
            metrics.warnings.push(`No child field found for parent ${parentField}`);
            return;
        }

        const childData = survey.childConfig[childField];
        if (!childData) {
            metrics.warnings.push(`No child data found for ${childField}`);
            return;
        }

        const childQuestion = survey.getQuestionByName(childField);
        if (!childQuestion) {
            metrics.warnings.push(`Child question ${childField} not found`);
            return;
        }

        const parentId = parentConfig.valueToIdMap[parentValue];
        const relevantChoices = [];
        const choicesData = childData.choices;

        choicesData.value.forEach((value, idx) => {
            if (choicesData.parentId[idx] === parentId) {
                relevantChoices.push({
                    value: value,
                    text: choicesData.text[idx]
                });
            }
        });

        childQuestion.choices = relevantChoices;

        if (childQuestion.value && !relevantChoices.some(c => c.value === childQuestion.value)) {
            childQuestion.value = null;
        }

        saveSurveyProgress(survey);
    }

    const attemptUpdate = (retries = 3) => {
        if (typeof survey === 'undefined' || survey === null) {
            if (retries > 0) {
                setTimeout(() => attemptUpdate(retries - 1), CHOICE_UPDATE_DELAY);
                return;
            }
            document.getElementById("surveyNotDefinedMessage").style.display = "block";
            metrics.errors.push("Survey not initialized after retries");
            console.error("Summary:", metrics);
            return;
        }

        survey.dynamicConfig = data;

        // Process standalone and child fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "standalone" || fieldData.type === "child") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    metrics.warnings.push(`Target question not found: ${fieldName}`);
                    return;
                }

                if (fieldData.type === "standalone") {
                    setChoices(targetQuestion, fieldData.choices);
                    metrics.processedFields.standalone++;
                } else {
                    processChildField(fieldName, fieldData);
                }
            }
        });

        // Process parent fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "param_parent" || fieldData.type === "choice_parent") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    metrics.warnings.push(`Target question not found: ${fieldName}`);
                    return;
                }

                setupParentHandler(fieldName, fieldData);

                if (fieldData.type === "choice_parent") {
                    setChoices(targetQuestion, fieldData.choices);
                }
            }
        });

        setTimeout(updateChildFieldsFromParents, CHOICE_UPDATE_DELAY);

        // Log final summary
        console.log("Dynamic choices update summary:", {
            ...metrics,
            totalProcessedFields: Object.values(metrics.processedFields).reduce((a, b) => a + b, 0),
            totalWarnings: metrics.warnings.length,
            totalErrors: metrics.errors.length
        });
    };

    attemptUpdate();
}

// Cookie handling functions remain the same
function saveSurveyProgress(survey) {
    try {
        if (!survey || !survey.data) {
            return;
        }

        const dataToStore = { ...survey.data };

        if (survey.dynamicConfig) {
            dataToStore._dynamicConfig = {
                parentFields: {},
                childChoices: {}
            };

            Object.entries(survey.parentConfig || {}).forEach(([parentField, config]) => {
                const parentQuestion = survey.getQuestionByName(parentField);
                if (parentQuestion) {
                    dataToStore._dynamicConfig.parentFields[parentField] = {
                        value: parentQuestion.value,
                        type: config.type,
                        choices: config.choicesList
                    };

                    const childField = config.childField;
                    if (childField) {
                        const childQuestion = survey.getQuestionByName(childField);
                        if (childQuestion) {
                            dataToStore._dynamicConfig.childChoices[childField] = {
                                choices: childQuestion.choices,
                                value: childQuestion.value,
                                parentField: parentField
                            };
                        }
                    }
                }
            });
        }

        setCookie(COOKIE_NAME, dataToStore, COOKIE_EXPIRATION_DAYS);
    } catch (error) {
        console.error("Error saving survey progress:", error);
    }
}

function restoreDynamicChoices(survey, savedData) {
    try {
        if (!savedData || !savedData._dynamicConfig) {
            return;
        }

        const { parentFields, childChoices } = savedData._dynamicConfig;

        Object.entries(parentFields).forEach(([parentField, config]) => {
            const parentQuestion = survey.getQuestionByName(parentField);
            if (parentQuestion) {
                if (config.type === "choice_parent" && config.choices) {
                    parentQuestion.choices = config.choices;
                }

                if (config.value !== undefined) {
                    parentQuestion.value = config.value;
                }
            }
        });

        Object.entries(childChoices).forEach(([childField, config]) => {
            const childQuestion = survey.getQuestionByName(childField);
            if (childQuestion && config.choices) {
                childQuestion.choices = config.choices;

                if (config.value !== undefined &&
                    config.choices.some(choice => choice.value === config.value)) {
                    childQuestion.value = config.value;
                }
            }
        });
    } catch (error) {
        console.error("Error restoring dynamic choices:", error);
    }
}

Shiny.addCustomMessageHandler("updateDynamicChoices", updateDynamicChoices);

function restoreDynamicChoices(survey, savedData) {
    if (!savedData || !savedData._dynamicConfig) return;

    console.log("Restoring dynamic choices:", savedData._dynamicConfig);
    const { parentFields, childChoices } = savedData._dynamicConfig;

    // First restore parent values
    Object.entries(parentFields).forEach(([parentField, config]) => {
        const parentQuestion = survey.getQuestionByName(parentField);
        if (parentQuestion) {
            console.log(`Restoring parent ${parentField} value:`, config.value);
            parentQuestion.value = config.value;
        }
    });

    // Then restore child choices and values
    Object.entries(childChoices).forEach(([childField, config]) => {
        const childQuestion = survey.getQuestionByName(childField);
        if (childQuestion) {
            console.log(`Restoring child ${childField} choices:`, config.choices);
            childQuestion.choices = config.choices;
            if (config.value !== undefined &&
                config.choices.some(choice => choice.value === config.value)) {
                childQuestion.value = config.value;
            }
        }
    });
}
