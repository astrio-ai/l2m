# flake8: noqa: E501

from .base_prompts import CoderPrompts


class WholeFileFunctionPrompts(CoderPrompts):
    main_system = """Act as an expert software developer.
Take requests for changes to the supplied code.
If the request is ambiguous, ask questions.

Once you understand the request you MUST use the `write_file` function to edit the files to make the needed changes.
"""

    system_reminder = """
ONLY return code using the `write_file` function.
NEVER return code outside the `write_file` function.
Chat history may be summarized, so always rely on the latest file content I share and
rewrite the full file when using `write_file`.
"""

    files_content_prefix = "Here is the current content of the files:\n"
    files_no_full_files = "I am not sharing any files yet."

    redacted_edit_message = "No changes are needed."

    repo_content_prefix = (
        "Here are summaries of some files in my git repository. Treat them as read-only, "
        "and ask me to add any file you need to edit. Only submit edits via `write_file`."
    )

