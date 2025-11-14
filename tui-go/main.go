package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// Styles
var (
	// Input box style - with background
	inputBoxStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B4B4B4")).
			Background(lipgloss.Color("#2b2b2b")).
			Padding(0, 1)

	// User input prompt style
	promptStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B4B4B4")).
			Bold(true)

	// Assistant output style
	assistantStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#787878"))

	// Error style
	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B45A5A"))

	// Separator style
	separatorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#505050"))
)

type model struct {
	input           string
	cursor          int
	messages        []message
	pythonCmd       *exec.Cmd
	pythonStdin     *bufio.Writer
	pythonStdout    *bufio.Reader
	waiting         bool
	err             error
	viewport        int
	terminalHeight  int
	terminalWidth   int
}

type message struct {
	role    string // "user" or "assistant"
	content string
}

type pythonResponseMsg struct {
	content string
	err     error
}

func initialModel() model {
	return model{
		input:          "",
		cursor:         0,
		messages:       []message{},
		waiting:        false,
		viewport:       0,
		terminalHeight: 24,
		terminalWidth:  80,
	}
}

func (m model) Init() tea.Cmd {
	return tea.Batch(
		tea.EnterAltScreen,
		startPythonBackend,
	)
}

// Start the Python L2M backend
func startPythonBackend() tea.Msg {
	// TODO: Implement Python backend communication
	// For now, just return success
	return pythonResponseMsg{content: "Backend ready (demo mode)"}
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.terminalHeight = msg.Height
		m.terminalWidth = msg.Width
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "ctrl+d":
			return m, tea.Quit

		case "enter":
			if m.input == "" {
				return m, nil
			}
			
			// Add user message
			m.messages = append(m.messages, message{
				role:    "user",
				content: m.input,
			})
			
			// TODO: Send to Python backend and get response
			// For now, simulate a response
			m.messages = append(m.messages, message{
				role:    "assistant",
				content: "This is a simulated response. Python backend integration coming next...",
			})
			
			m.input = ""
			m.cursor = 0
			m.waiting = false
			return m, nil

		case "backspace":
			if m.cursor > 0 {
				m.input = m.input[:m.cursor-1] + m.input[m.cursor:]
				m.cursor--
			}
			return m, nil

		case "left":
			if m.cursor > 0 {
				m.cursor--
			}
			return m, nil

		case "right":
			if m.cursor < len(m.input) {
				m.cursor++
			}
			return m, nil

		default:
			// Regular character input
			if len(msg.String()) == 1 {
				m.input = m.input[:m.cursor] + msg.String() + m.input[m.cursor:]
				m.cursor++
			}
			return m, nil
		}

	case pythonResponseMsg:
		if msg.err != nil {
			m.err = msg.err
			return m, nil
		}
		return m, nil
	}

	return m, nil
}

func (m model) View() string {
	var output strings.Builder

	// Display messages
	if len(m.messages) > 0 {
		for i, msg := range m.messages {
			if msg.role == "user" {
				output.WriteString(promptStyle.Render("> "))
				output.WriteString(msg.content)
			} else {
				output.WriteString(separatorStyle.Render("▌"))
				output.WriteString("\n\n")
				output.WriteString(assistantStyle.Render(msg.content))
			}
			
			if i < len(m.messages)-1 {
				output.WriteString("\n\n")
			}
		}
		output.WriteString("\n\n")
	}

	// Display error if any
	if m.err != nil {
		output.WriteString(errorStyle.Render(fmt.Sprintf("Error: %v\n\n", m.err)))
	}

	// Display input box with background
	inputLine := promptStyle.Render("> ") + m.input
	if m.cursor == len(m.input) {
		inputLine += "█" // Cursor at end
	}
	
	// Apply background to the entire input box
	styledInput := inputBoxStyle.Render(inputLine)
	output.WriteString(styledInput)
	
	// Help text
	output.WriteString("\n\n")
	output.WriteString(lipgloss.NewStyle().
		Foreground(lipgloss.Color("#505050")).
		Render("Ctrl+C/Ctrl+D to quit"))

	return output.String()
}

func main() {
	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

